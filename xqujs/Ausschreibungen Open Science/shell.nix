
{ pkgs ? import <nixpkgs> {} }:

pkgs.mkShell {
  buildInputs = with pkgs; [
    R
    rPackages.stringr
    rPackages.tidyverse
  ];

  shellHook = ''
    export R_LIBS_USER="/var/empty"
    export R_LIBS=""
    export LANG="en_US.UTF-8"
  '';
}
