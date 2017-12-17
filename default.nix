{ nixpkgs ? import <nixpkgs> {}, compiler ? "default", build ? false }:

let

  inherit (nixpkgs) pkgs;

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  distSourceFilter = name: type: let baseName = baseNameOf (toString name); in ! (
    (type == "directory" && (baseName == "dist" || baseName == ".git")) ||
    false
    );

  lift = haskellPackages.callPackage
      ({ mkDerivation, aeson, base, binary, bytestring, containers
      , deepseq, directory, filepath, hashable, heap
      , lens, mtl, optparse-applicative, pqueue, pretty-show
      , process, psqueues, random, stdenv, stm, text, time
      , transformers, unix, cabal-install, haskdogs, hasktags
      , flippers, tasty, tasty-quickcheck, tasty-hunit, QuickCheck
      , concurrent-extra
      }:
      mkDerivation {
        pname = "lift";
        version = "0.1.0.0";
        src = builtins.filterSource distSourceFilter ./.;
        isLibrary = true;
        isExecutable = true;
        doHaddock = false;
        libraryHaskellDepends = [
          aeson base binary bytestring containers deepseq directory filepath
          hashable heap lens
          mtl optparse-applicative pqueue
          pretty-show process psqueues random stm text time
          transformers unix cabal-install flippers
          tasty tasty-quickcheck tasty-hunit QuickCheck concurrent-extra
        ];
        executableHaskellDepends = [
          aeson base binary bytestring containers lens mtl
          optparse-applicative text unix haskdogs hasktags
          ] ;

        license = stdenv.lib.licenses.mit;

        shellHook=''
          if test -f /etc/myprofile ; then
            . /etc/myprofile
          fi

          if test -f profile.sh ; then
            . profile.sh
          fi
        '';
      }) {};

in

  if pkgs.lib.inNixShell && !build then lift.env else lift
