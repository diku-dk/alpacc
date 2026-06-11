let
  pkgs = import <nixpkgs> { config.allowUnfree = true; };
  compilerVersion = "ghc910"; 
  compiler = pkgs.haskell.packages."${compilerVersion}";
in
let pkg =
      compiler.developPackage {
        root = ./.;
        modifier = drv:
          pkgs.haskell.lib.addBuildTools drv
            ((with pkgs.haskellPackages;
              [ cabal-install
                ghcid
                haskell-language-server
              ]) ++
            (with pkgs;
              [ python3
                gcc
                gmp
                ispc
                rustc
                cargo
                mkjson
                parallel
              ]));
      };
in pkg
