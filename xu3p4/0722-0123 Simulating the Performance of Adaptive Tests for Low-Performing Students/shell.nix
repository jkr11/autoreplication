
{ pkgs ? import <nixpkgs> {} }:

pkgs.mkShell {
  buildInputs = with pkgs; [
    R
    rPackages.tidyverse rPackages.pairwise rPackages.TAM rPackages.catR
  ];

  shellHook = ''
    export R_LIBS_USER="/var/empty"
    export R_LIBS=""
  '';
}
