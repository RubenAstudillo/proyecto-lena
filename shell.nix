{ nixpkgs ? import <nixpkgs> {}, compiler ? "default" }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, base, hmatrix, hspec, JuicyPixels, stdenv
      , vector, vector-fftw
      }:
      mkDerivation {
        pname = "proyecto-lena";
        version = "0.1.0.0";
        src = ./.;
        isLibrary = true;
        isExecutable = true;
        libraryHaskellDepends = [
          base hmatrix JuicyPixels vector vector-fftw
        ];
        executableHaskellDepends = [ base ];
        testHaskellDepends = [ base hspec JuicyPixels vector ];
        description = "FFT en algunas imagenes";
        license = stdenv.lib.licenses.bsd3;
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  drv = haskellPackages.callPackage f {};

in

  if pkgs.lib.inNixShell then drv.env else drv
