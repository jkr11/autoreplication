
{ pkgs ? import <nixpkgs> {} }:

pkgs.mkShell {
  buildInputs = with pkgs; [
    R
    rPackages.stringr rPackages.tidyverse
  ];
}
