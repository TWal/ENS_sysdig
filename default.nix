{ nixpkgs ? import <nixpkgs> {}, compiler ? "ghc801" }:
let
  inherit (nixpkgs) pkgs;
  ghc = pkgs.haskell.packages.${compiler}.ghcWithPackages (ps: with ps; [
        ]);
in pkgs.stdenv.mkDerivation {
  name = "netlist-writer";
  buildInputs = [ ghc ];
  shellHook = "eval $(egrep ^export ${ghc}/bin/ghc";
}

