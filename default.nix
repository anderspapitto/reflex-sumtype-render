let
  hostpkgs = import <nixpkgs> {};
  reflexPkgs = import ./reflex-platform { };
  pkgs = reflexPkgs.nixpkgs;

  ghc-env = reflexPkgs.ghc.ghcWithPackages (p: with p; [
      intero generics-sop reflex-dom
    ]);
  ghcjs-env = reflexPkgs.ghcjs.ghcWithPackages (p: with p; [
      generics-sop reflex-dom
    ]);
in pkgs.buildEnv {
  name = "env-reflex-sumtype-render";
  paths = [
      ghc-env
      ghcjs-env
      pkgs.haskellPackages.cabal-install
      pkgs.haskellPackages.stack
      pkgs.nodejs
    ];
}
