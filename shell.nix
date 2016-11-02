{ nixpkgs ? import <nixpkgs> {}, compiler ? "default" }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, base, hmatrix, hspec, hstatistics
      , JuicyPixels, mtl, QuickCheck, stdenv, vector, vector-algorithms
      , vector-fftw
      }:
      mkDerivation {
        pname = "proyecto-lena";
        version = "0.1.0.0";
        src = ./.;
        isLibrary = true;
        isExecutable = true;
        libraryHaskellDepends = [
          base hmatrix hstatistics JuicyPixels mtl vector vector-algorithms
          vector-fftw
        ];
        executableHaskellDepends = [ base ];
        testHaskellDepends = [
          base hmatrix hspec JuicyPixels mtl QuickCheck vector
        ];
        description = "FFT en algunas imagenes";
        license = stdenv.lib.licenses.bsd3;
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  drv = haskellPackages.callPackage f {};

in

  if pkgs.lib.inNixShell then drv.env else drv
