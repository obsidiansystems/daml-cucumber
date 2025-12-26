{ nix-daml-sdk ? import ../nix/nix-daml-sdk { sdkVersion = "2.6.5"; }
}:
let
  nixThunk = import ../nix/nix-thunk {};
  inherit (nixThunk) thunkSource;
  pkgs = import ../nix/nixpkgs {};
  src = builtins.filterSource (path: type: !(builtins.elem (baseNameOf path) [
      "release.nix"
      ".git"
      "dist"
      "cabal.haskell-ci"
      "cabal.project"
      ".travis.yml"
    ])) ./.;
  haskellLib = pkgs.haskell.lib;
  haskellPackages = pkgs.haskellPackages.override {
    overrides = self: super: {
      abacate = self.callCabal2nix "abacate" (thunkSource ./dep/abacate) {};
      coquina = self.callCabal2nix "coquina" (thunkSource ./dep/coquina) {};
      reflex-vty = self.callCabal2nix "reflex-vty" (thunkSource ./dep/reflex-vty) {};
      directory-contents = self.callCabal2nix "directory-contents" (thunkSource ./dep/directory-contents) {};
      logging-effect-colors = self.callCabal2nix "logging-effect-colors" (thunkSource ./dep/logging-effect-colors) {};

      daml-cucumber = haskellLib.overrideCabal
        (self.callCabal2nix "daml-cucumber" src {})
        (drv: {
          librarySystemDepends = (drv.librarySystemDepends or []) ++ [ nix-daml-sdk.sdk ];
        });
      reflex-process = self.callCabal2nix "reflex-process" (thunkSource ./dep/reflex-process) {};
    };
  };
  ghc = haskellPackages.ghc;
in
  { inherit (haskellPackages) daml-cucumber;
    inherit haskellPackages ghc nix-daml-sdk;
  }
