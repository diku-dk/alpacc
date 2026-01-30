let
  pkgs = import <nixpkgs> { config.allowUnfree = true; };
  compilerVersion = "ghc98"; 
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
                cudaPackages.cuda_cudart
                cudaPackages.cuda_nvcc
                (lib.getLib cudaPackages.cuda_nvrtc)
                (lib.getDev cudaPackages.cuda_nvrtc)
                (lib.getLib cudaPackages.cuda_cudart)
                (lib.getDev cudaPackages.cuda_cudart)
                (lib.getStatic cudaPackages.cuda_cudart)
                libGLU libGL
                xorg.libXi xorg.libXmu freeglut
                xorg.libXext xorg.libX11 xorg.libXv xorg.libXrandr zlib 
                ncurses5 binutils
                opencl-headers
                clinfo
                gcc11
                gmp
                ispc
                ocl-icd
                rustc
                cargo
                mkjson
                parallel
              ]));
      };
in pkg.overrideAttrs (attrs: {
  shellHook = attrs.shellHook + ''
    export CUDA_PATH=${pkgs.cudatoolkit}
    export CPATH=${pkgs.cudatoolkit}/include
    export LIBRARY_PATH=${pkgs.cudatoolkit}/lib:${pkgs.lib.getStatic pkgs.cudaPackages.cuda_cudart}/lib
    export LD_LIBRARY_PATH=${pkgs.cudatoolkit}/lib:${pkgs.linuxPackages.nvidia_x11}/lib:${pkgs.lib.getStatic pkgs.cudaPackages.cuda_cudart}/lib
  '';
})
