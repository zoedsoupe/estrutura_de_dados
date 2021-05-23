{ pkgs ? import <nixpkgs> {} }:

with pkgs;

let
  hpkgs = with haskellPackages; [
    brittany
  ];
in mkShell {
  buildInputs = [ ghc cabal-install stack readline ] ++ hpkgs;

  shellHook = "echo 'SEJA BEM VINDE!'";
}
