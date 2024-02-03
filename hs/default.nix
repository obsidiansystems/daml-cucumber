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
      abacate = haskellLib.doJailbreak (haskellLib.markUnbroken super.abacate);
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
      lsp-types = self.callHackageDirect {
        pkg = "lsp-types";
        ver = "2.1.0.0";
        sha256 = "0xnrx4x3y2ldly277qbp3xk2rkaxsnpl26h4ir3d8n4z8n8hxqmf";
      } {};
      lsp = self.callHackageDirect {
        pkg = "lsp";
        ver = "2.3.0.0";
        sha256 = "128vknywgf9skk3dccqdqbjpl47anvqzllpr19l6q97nnnk2z6di";
      } {};
      lsp-test = haskellLib.dontCheck (self.callHackageDirect {
        pkg = "lsp-test";
        ver = "0.16.0.1";
        sha256 = "0pk5ganrija7ds1y68yg81r0n8hfjgy80zq1rqv3z4q9gq7zsrji";
      } {});
      lens-aeson = haskellLib.doJailbreak super.lens-aeson;
      dependent-sum-template = self.callHackageDirect {
        pkg = "dependent-sum-template";
        ver = "0.2.0.1";
        sha256 = "123chg589dcp2854rfkydb8cwkvy6abjb9wp4mxazb01w4b21v5a";
      } {};
    };
  };
in
  { inherit (ghc) daml-cucumber;
    inherit ghc nix-daml-sdk;
  }
