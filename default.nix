let
  platform = import ./nix/reflex-platform {};
  pkgs = platform.nixpkgs;
  hsBuild = import ./hs {};
  versions = builtins.map (x: let
    version = builtins.replaceStrings [ ".json" ] [ "" ] x;
    versionS = builtins.replaceStrings [ "." ] [ "" ] version;
  in {
    name = "daml-" + versionS;
    value = rec {
      recurseForDerivations = true;
      damlSdk = import ./nix/nix-daml-sdk { sdkVersion = version; };
      container = mkContainer damlSdk version;
      inherit version;
    };
  })
    (builtins.attrNames (builtins.readDir ((pkgs.hackGet ./nix/nix-daml-sdk) + "/versions")));

  mkContainer = nix-daml-sdk: version: pkgs.dockerTools.buildImage {
    name = "daml-cucumber";
    tag = version;

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
  };

  outputs = builtins.listToAttrs versions;
in {
  container = outputs.daml-265.container;
  recurseForDerivations = true;
} // outputs
