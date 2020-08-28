let
  pkgs = import <nixpkgs> {};
in
with pkgs;
mkShell
  {
    inputsFrom = with pkgs;
      [
        (let
          ihaskell = fetchFromGitHub
            {
              owner = "gibiansky";
              repo = "IHaskell";
              rev = "36f0acdeaabf908265aed4ce7c5b0333db1ee815";
              sha256 = "18rcsxz6gsrpnapgfdiis1k376147bn2hk591isj22f6f0b9fhn6";
            };
         in import "${ihaskell}/release.nix"
           {
             compiler = "ghc883";
             nixpkgs = pkgs;
             packages = self: with self;
               [
                 ihaskell
                 ihaskell-aeson
                 # ihaskell-basic
                 ihaskell-blaze
                 # ihaskell-charts
                 # ihaskell-diagrams
                 # ihaskell-display
                 # ihaskell-gnuplot
                 ihaskell-graphviz
                 ihaskell-hatex
                 # ihaskell-hvega
                 # ihaskell-inline-r
                 ihaskell-juicypixels
                 ihaskell-magic
                 # ihaskell-parsec
                 # ihaskell-plot
                 # ihaskell-rlangqq
                 # ihaskell-widgets
               ];
           }
        )
      ];

    shellHook =
      ''
         export EDITOR="$(emacs)";
      '';

  } # end of mkShell