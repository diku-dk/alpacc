let
  pkgs = import <nixpkgs> { config.allowUnfree = true; };
  unstable = import <nixos-unstable> { };
  compilerVersion = "ghc94"; 
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
                git gitRepo gnupg autoconf curl
                procps gnumake util-linux m4 gperf unzip
                cudatoolkit linuxPackages.nvidia_x11
                libGLU libGL
                xorg.libXi xorg.libXmu freeglut
                xorg.libXext xorg.libX11 xorg.libXv xorg.libXrandr zlib 
                ncurses5 binutils
                opencl-headers
                clinfo
           #     gcc11
           #     stdenv.cc
                gmp
                ispc
                ocl-icd
                rustc
                cargo
              ]) ++
            (with unstable;
              [ futhark
                mkjson
              ]));
      };
in pkg.overrideAttrs (attrs: {
  shellHook = attrs.shellHook + ''
    export CUDA_PATH=${pkgs.cudatoolkit}
    # export LD_LIBRARY_PATH=${pkgs.linuxPackages.nvidia_x11}/lib:${pkgs.ncurses5}/lib
    export EXTRA_LDFLAGS="-L/lib -L${pkgs.linuxPackages.nvidia_x11}/lib"
    export EXTRA_CCFLAGS="-I/usr/include"
  '';
})
