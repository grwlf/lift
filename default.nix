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
      ({ mkDerivation, stdenv, base, containers, mtl, optparse-applicative, stm,
      transformers, cabal-install, concurrent-extra
      }:
      mkDerivation {
        pname = "lift";
        version = "0.1.0.0";
        src = builtins.filterSource distSourceFilter ./.;
        isLibrary = true;
        isExecutable = true;
        doHaddock = false;
        libraryHaskellDepends = [
          base containers mtl optparse-applicative stm transformers
          cabal-install concurrent-extra
        ];

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
