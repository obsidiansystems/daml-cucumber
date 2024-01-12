{}:
let
  platform =  import ../nix/reflex-platform {};
  pkgs = platform.nixpkgs;
in
  pkgs.mkShell {
    name = "daml-cucumber-shell";
    buildInputs = [
      pkgs.cabal-install
      pkgs.ghcid
    ];
    inputsFrom = [
      (import ./. {}).daml-cucumber.env
    ];
  }
