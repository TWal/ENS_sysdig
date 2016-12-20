{ nixpkgs ? import <nixpkgs> {} }:
let
  inherit (nixpkgs) pkgs;
  packages = with pkgs; [ gnumake gcc flex bison ];
in pkgs.stdenv.mkDerivation {
  name = "sysdig-asm";
  buildInputs =  packages;
}

