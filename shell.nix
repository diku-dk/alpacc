with import <nixpkgs> { };
pkgs.mkShell {
  buildInputs = [
    python3
    futhark
    git
    python310Packages.mypy
    python310Packages.black
    python310Packages.pip
    python310Packages.numpy
    python310Packages.virtualenv
    haskell.compiler.ghc927
    cabal-install
  ];
}