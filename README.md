Installation
============

On systems with *Nix* package manager installed, type `nix-shell` to enter
development shell, then type `./ghci.sh` to use interactive prompt or `ghc
src/Main.hs` to compile the `src/Main` binary

On non-Nix system, you need to bring up a Haskell environment with the
following packages installed:

    containers, mtl, optparse-applicative, stm, transformers, concurrent-extra

*HaskellPlatform* should already include all listed packages except maybe the
last one.
