with import <nixpkgs> { };
pkgs.mkShell {
  shellHook = ''
    export PATH=${toString ./.}
  '';
  buildInputs = [
    python3
    python3Packages.mypy
    python3Packages.black
    python3Packages.waitress
    haskell.compiler.ghc927
    cabal-install
    futhark
  ];
}