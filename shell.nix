{ nixpkgs ? import <nixpkgs> {}, compiler ? "default" }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, base, largeword, stdenv }:
      mkDerivation {
        pname = "largeword-lens";
        version = "0.1.0.0";
        src = ./.;
        libraryHaskellDepends = [ base largeword ];
        homepage = "https://github.com/nickspinale/largeword-lens.git";
        license = stdenv.lib.licenses.mit;
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  drv = haskellPackages.callPackage f {};

in

  if pkgs.lib.inNixShell then drv.env else drv
