
{ pkgs ? import <nixpkgs> {} }:

pkgs.mkShell {
  buildInputs = with pkgs; [
    R
    rPackages.tidyverse rPackages.WrightMap rPackages.RColorBrewer rPackages.sfsmisc rPackages.pairwise rPackages.TAM
  ];

  shellHook = ''
    export R_LIBS_USER="/var/empty"
    export R_LIBS=""
  '';
}
