
{ pkgs ? import <nixpkgs> {} }:

pkgs.mkShell {
  buildInputs = with pkgs; [
    R
    rPackages.TAM
    rPackages.catR
    rPackages.pairwise
    rPackages.tidyverse
  ];

  shellHook = ''
    export R_LIBS_USER="/var/empty"
    export R_LIBS=""
  '';
}
