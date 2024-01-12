{}:
let
  platform =  import ../nix/reflex-platform {};
  pkgs = platform.nixpkgs;
  damlSdk = import ../nix/nix-daml-sdk {};
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
      which = haskellLib.doJailbreak super.which;
      reflex = self.callCabal2nix "reflex" (pkgs.hackGet ./dep/reflex) {};
      daml-cucumber = haskellLib.overrideCabal
        (self.callCabal2nix "daml-cucumber" src {})
        (drv: {
          librarySystemDepends = (drv.librarySystemDepends or []) ++ [ damlSdk.sdk ];
        });
    };
  };
in
  { daml-cucumber = ghc.daml-cucumber;
    inherit ghc;
  }
