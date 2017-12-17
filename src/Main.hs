{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
module Main where

import Control.Concurrent (forkIO, killThread)
import Control.Concurrent.STM (STM(..), atomically)
import Control.Concurrent.STM.TChan (TChan, readTChan, writeTChan, tryReadTChan, newTChanIO)
import Control.Concurrent.Event
import Control.Exception hiding (assert)
import Control.Monad
import Control.Monad.State
import Data.Char (ord)
import Data.Set (Set(..),member,insert,delete,minView)
import Data.Time
import Data.Monoid ((<>))
import Options.Applicative
import System.IO
import System.Exit
import Text.Printf

data Config = Config {
  cfg_nfloors :: Integer
, cfg_hfloor_m :: Rational
, cfg_speed_ms :: Rational
, cfg_tdoors_s :: Rational
} deriving(Show)

defConfig :: Config
defConfig = Config 9 3.0 3.0 1

getConfig :: IO Config
getConfig = execParser (info ((
  Config
    <$> (read <$> option str (long "nfloors" <> short 'n' <> value "5"))
    <*> (readD <$> option str (long "hfloor" <> short 'h' <> value "3"))
    <*> (readD <$> option str (long "speed" <> short 's' <> value "3"))
    <*> (readD <$> option str (long "tdoors" <> short 't' <> value "2"))
  ) <**> helper) idm)
  where readD str = toRational (read str :: Double)

data DoorState = DoorOpened | DoorClosed
  deriving(Show,Eq,Ord)

data LiftState = LiftState {
  ls_ftgt :: Maybe Integer
, ls_fcurr :: Integer
, ls_door :: DoorState
} deriving(Show,Eq,Ord)

data EnvState = EnvState {
  es_cknobs :: Set Integer
, es_fknobs :: Set Integer
} deriving(Show,Eq,Ord)

initEnvState = EnvState mempty mempty
initLiftState = LiftState Nothing 1 DoorClosed

stepEnv :: LiftState -> EnvState -> EnvState
stepEnv (LiftState _ x DoorOpened) EnvState{..} =
  EnvState (delete x es_cknobs) (delete x es_fknobs)
stepEnv _ es = es

stepLift :: Config -> LiftState -> EnvState -> (LiftState,Rational)
stepLift Config{..} ls@LiftState{..} EnvState{..} =
  let
    -- TODO: improve selection rule
    tgt' = msum [ls_ftgt, fst<$>(minView es_cknobs), fst<$>(minView es_fknobs)]
    tfloor = cfg_hfloor_m/cfg_speed_ms
  in
  case ls of
    LiftState x@(Just tgt) curr DoorClosed
      |tgt==curr -> (LiftState Nothing curr DoorOpened, 0.5)
      |tgt>curr  -> (LiftState x (curr+1) DoorClosed, tfloor)
      |tgt<curr  -> (LiftState x (curr-1) DoorClosed, tfloor)
    LiftState Nothing curr DoorClosed
      | tgt'==Nothing -> (LiftState Nothing curr DoorClosed, 999)
      | otherwise -> (LiftState tgt' curr DoorClosed, 0.5)
    LiftState x curr DoorOpened ->
      (LiftState Nothing curr DoorClosed, cfg_tdoors_s)

drawLift :: Config -> LiftState -> EnvState -> IO ()
drawLift Config{..} LiftState{..} EnvState{..} = do
  printf "\n"
  forM_ (reverse [1..cfg_nfloors]) $ \f ->
    let
      draw_knob knb = if f`member`knb then "*" else "o"
      draw_lift = if ls_fcurr == f then (if ls_door == DoorOpened then "[  ]"  else " [] ") else "...."
    in
    printf $ (printf "%02d" f) <> ":" <> (draw_knob es_cknobs) <> " " <> draw_lift <> " " <> (draw_knob es_fknobs) <> "\n"

whileM_ :: (Monad m) => a -> (a -> m (Maybe a)) -> m ()
whileM_ a0 m = do
  x <- m a0
  case x of
    Just a' -> whileM_ a' m
    Nothing -> return ()

drainChannel :: TChan a -> STM [a]
drainChannel tc = go [] where
  go buf = do
    x <- tryReadTChan tc
    case x of
      Nothing -> return (reverse buf)
      Just y -> go (y:buf)

applyInput :: [Either Integer Integer] -> EnvState -> EnvState
applyInput inp es@EnvState{..} = foldr apply es inp where
  apply (Left x) es = es{es_cknobs = insert x es_cknobs}
  apply (Right x) es = es{es_fknobs = insert x es_fknobs}

clearTerminal :: (MonadIO m) => m ()
clearTerminal = liftIO $ do
  putStrLn "\027[2J"
  putStrLn "\027[1;1H"

main :: IO ()
main = do
  hSetBuffering stdin NoBuffering
  hSetBuffering stdout NoBuffering

  config@Config{..} <- getConfig
  when (cfg_nfloors<5 || cfg_nfloors>20) $ do
    printf "Invalid 'nfloor' argument, check --help" >> exitFailure
  when (cfg_hfloor_m<0) $ do
    printf "Invalid 'hfloor' argument, check --help" >> exitFailure
  when (cfg_speed_ms<=0) $ do
    printf "Invalid 'speed' argument, check --help" >> exitFailure
  when (cfg_tdoors_s<0) $ do
    printf "Invalid 'tdoors' argument, check --help" >> exitFailure

  chan <- newTChanIO
  event <- new
  tid <- forkIO $ do
    flip evalStateT (initLiftState,initEnvState) $ do
      forever $ do
        (ls,es0) <- get
        (ls2,delay) <- pure $ stepLift config ls es0
        t1 <- (\t -> (fromRational delay) `addUTCTime` t) <$> (liftIO getCurrentTime)
        whileM_ () $ \_ -> do
          (_,es) <- get
          to <- (\t0 -> round $ 1.0e6 * (diffUTCTime t1 t0)) <$> (liftIO getCurrentTime)
          liftIO $ clearTerminal
          liftIO $ putStrLn "Use letters 'a'..'z' to activate cabin buttons (numbers also work here)"
          liftIO $ putStrLn "Use letters 'A'..'Z' to activate floor buttons"
          liftIO $ putStrLn "Press '.' or Ctrl+C to exit"
          liftIO $ drawLift config ls es
          event_hit <- liftIO $ waitTimeout event to
          inp <- liftIO $ atomically $ drainChannel chan
          case event_hit of
            False -> do
              es2 <- pure $ stepEnv ls2 (applyInput inp es)
              put (ls2, es2)
              return Nothing
            True -> do
              put (ls, applyInput inp es)
              return (if ls2==ls then Nothing else Just ())

  handle (\(e::SomeException) -> return ()) $ do
    whileM_ () $ \_ -> do
      c <- liftIO getChar
      case c of
        '.' -> do
          return Nothing
        c ->
          let
            inp x = take (fromInteger cfg_nfloors) x
          in do
          when (c`elem`(inp ['a'..'z'])) $ do
            atomically $ writeTChan chan (Left $ toInteger $ ord c - ord 'a' + 1)
          when (c`elem`(inp ['A'..'Z'])) $ do
            atomically $ writeTChan chan (Right $ toInteger $ ord c - ord 'A' + 1)
          when (c`elem`(inp ['1'..'9'])) $ do
            atomically $ writeTChan chan (Left $ toInteger $ ord c - ord '0')
          signal event
          return (Just ())

  printf "Exiting\n"
  killThread tid


