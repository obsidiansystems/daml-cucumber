{}:
let
  pkgs = import ./nix/nixpkgs {};
  hs = import ./hs {};
in
  pkgs.mkShell {
    name = "daml-cucumber-test";
    buildInputs = [
      pkgs.cabal-install
      pkgs.ghcid
      hs.nix-daml-sdk.vscode
      hs.nix-daml-sdk.canton
    ];
    inputsFrom = [
      hs.daml-cucumber.env
    ];
  }
