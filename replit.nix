{ pkgs }: {
  deps = [
    pkgs.gfortran
    # This ls available on nix is for XL fortran 
    # pkgs.fortran-language-server
    pkgs.fortls
  ];
}