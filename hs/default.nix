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
      neat-interpolation = haskellLib.doJailbreak super.neat-interpolation;
      # lsp-client = haskellLib.doJailbreak (self.callHackageDirect {
      #   pkg = "lsp-client";
      #   ver = "0.1.0.0";
      #   sha256 = "sha256-+tbJzuoe6aF+KDvxRmDnKNdPtkJM4pXwEH0dYAe2uKE=";
      # }{});
      # lens-aeson = haskellLib.doJailbreak super.lens-aeson;
      # lsp-types = self.callHackageDirect {
      #   pkg = "lsp-types";
      #   ver = "1.6.0.0";
      #   sha256 = "sha256-QSixsrCvsWlckG/LLF1z8LsHhqaXxVAxOPIA1NxjVT4=";
      # }{};
      # lsp = self.callHackageDirect {
      #   pkg = "lsp";
      #   ver = "1.6.0.0";
      #   sha256 = "sha256-g5R34SVz0kRD5zpODNsaaaIJOHty10cTS6ZDPi4s8pc=";
      # }{};
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
