{ nix-daml-sdk ? import ../nix/nix-daml-sdk { sdkVersion = "2.6.5"; }
}:
let
  platform =  import ../nix/reflex-platform {};
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
      abacate = self.callCabal2nix "abacate" (pkgs.hackGet ./dep/abacate) {};
      reflex-process = self.callCabal2nix "reflex-process" (pkgs.hackGet ./dep/reflex-process) {};
      which = haskellLib.doJailbreak super.which;
      reflex = self.callCabal2nix "reflex" (pkgs.hackGet ./dep/reflex) {};
      neat-interpolation = haskellLib.doJailbreak super.neat-interpolation;
      coquina = haskellLib.markUnbroken (haskellLib.doJailbreak super.coquina);
      logging-effect = self.callHackage "logging-effect" "1.4.0" {};
      daml-cucumber = haskellLib.overrideCabal
        (self.callCabal2nix "daml-cucumber" src {})
        (drv: {
          librarySystemDepends = (drv.librarySystemDepends or []) ++ [ nix-daml-sdk.sdk ];
        });
      logging-effect-colors = self.callHackage "logging-effect-colors" "0.1.0.0" {};
    };
  };
in
  { inherit (ghc) daml-cucumber;
    inherit ghc nix-daml-sdk;
  }
