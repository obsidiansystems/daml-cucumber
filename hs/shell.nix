{}:
let
  platform =  import ./reflex-platform {};
  pkgs = platform.nixpkgs;
  src = builtins.filterSource (path: type: !(builtins.elem (baseNameOf path) [
      "release.nix"
      ".git"
      "dist"
      "cabal.haskell-ci"
      "cabal.project"
      ".travis.yml"
    ])) ./.;
  haskellLib = platform.nixpkgs.haskell.lib;
  ghc = platform.ghc.override {
    overrides = self: super: {
      abacate = haskellLib.doJailbreak (haskellLib.markUnbroken super.abacate);
      reflex-process = self.callCabal2nix "reflex-process" (pkgs.hackGet ./dep/reflex-process) {};
    };
  };
in
  pkgs.mkShell {
    name = "daml-cucumber-shell";
    buildInputs = [
      pkgs.cabal-install
      pkgs.ghcid
    ];
    inputsFrom = [
      (ghc.callCabal2nix "daml-cucumber" src {}).env
    ];
  }
