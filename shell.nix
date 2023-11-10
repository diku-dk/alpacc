let
  pkgs = import <nixpkgs> { }; # pin the channel to ensure reproducibility!
  compilerVersion = "ghc92"; 
  compiler = pkgs.haskell.packages."${compilerVersion}";
in
compiler.developPackage {
  root = ./.;
  modifier = drv:
    pkgs.haskell.lib.addBuildTools drv (with pkgs.haskellPackages;
      [ cabal-install
        ghcid
        haskell-language-server
      ]);
}
