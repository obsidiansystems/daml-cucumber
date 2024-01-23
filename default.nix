let
  platform = import ./nix/reflex-platform {};
  pkgs = platform.nixpkgs;
  nix-daml-sdk = import ./nix/nix-daml-sdk {};
  hsBuild = import ./hs {};
in pkgs.dockerTools.buildImage {
  name = "daml-cucumber";
  tag = "latest";

  copyToRoot = pkgs.buildEnv {
    name = "image-root";
    paths = with nix-daml-sdk; [
      sdk
      pkgs.coreutils
      (pkgs.haskell.lib.justStaticExecutables hsBuild.daml-cucumber)
    ];
    pathsToLink = [ "/bin" "/share" "/lib" ];
  };

  config = {
    Env = [
      "PATH=/bin"
    ];
  };
}
