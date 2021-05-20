{ pkgs ? import <nixpkgs> {} }:

with pkgs;

mkShell {
  buildInputs = [ ghc stack readline ];

  shellHook = "SEJA BEM VINDE!";
}
