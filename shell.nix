{}:
let
  platform =  import ./nix/reflex-platform {};
  pkgs = platform.nixpkgs;
  nix-daml-sdk = import ./nix/nix-daml-sdk {};
in
  pkgs.mkShell {
    name = "daml-cucumber-test";
    buildInputs = [
      pkgs.cabal-install
      pkgs.ghcid
      nix-daml-sdk.sdk
      nix-daml-sdk.vscode
      nix-daml-sdk.canton
    ];
    inputsFrom = [
      (import ./hs {}).daml-cucumber.env
    ];
  }
