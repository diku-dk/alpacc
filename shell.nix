with import <nixpkgs> { };
pkgs.mkShell {
  buildInputs = [
    python3
    python3Packages.mypy
    python3Packages.black
    python3Packages.futhark-ffi
    python3Packages.numpy
    haskell.compiler.ghc927
    cabal-install
    futhark
  ];
}