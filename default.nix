let
  pkgs = import ./pkgs.nix;
in import "${pkgs.ihaskell}/release.nix" {
  compiler = "ghc865";
  nixpkgs = import pkgs.nixpkgs {};
  packages = self: with self; [ 
    hspec
    QuickCheck
    checkers
    hedis
    scotty
    trifecta
    raw-strings-qq
    generic-random
    graphviz
  ];
}
