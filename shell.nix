let
  pkgs = import <nixpkgs> { }; # pin the channel to ensure reproducibility!
  unstable = import <nixos-unstable> { };
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
        pkgs.python3
        unstable.futhark
      ]);
}
