let
  platform = import ./nix/reflex-platform {};
  pkgs = platform.nixpkgs;
  versions = builtins.map (x: let
    version = builtins.replaceStrings [ ".json" ] [ "" ] x;
    versionS = builtins.replaceStrings [ "." ] [ "" ] version;
  in {
    name = "daml-" + versionS;
    value = rec {
      recurseForDerivations = true;
      damlSdk = import ./nix/nix-daml-sdk { sdkVersion = version; };
      hsBuild = import ./hs { nix-daml-sdk = damlSdk; };
      damlLib = pkgs.stdenvNoCC.mkDerivation {
        name = "daml-cucumber";
        src = pkgs.lib.cleanSource ./daml;
        buildInputs = [ damlSdk.jdk damlSdk.sdk ];
        buildPhase = ''
          mkdir dist
          daml build -o dist.dar
        '';
        installPhase = ''
          mkdir $out
          cp dist.dar $out/
        '';
      }
      container = mkContainer damlSdk hsBuild version;
      inherit version;
    };
  })
    (builtins.attrNames (builtins.readDir ((pkgs.hackGet ./nix/nix-daml-sdk) + "/versions")));

  mkContainer = nix-daml-sdk: hsBuild: version: pkgs.dockerTools.buildImage {
    name = "daml-cucumber";
    tag = version;

    copyToRoot = pkgs.buildEnv {
      name = "image-root";
      paths = with nix-daml-sdk; [
        sdk
        pkgs.coreutils
        pkgs.bash
        (pkgs.haskell.lib.justStaticExecutables hsBuild.daml-cucumber)
      ];
      pathsToLink = [ "/bin" "/share" "/lib" ];
    };

    config = {
      Env = [
        "PATH=/bin"
      ];
      Cmd = [
        "/bin/sh"
      ];
    };
  };

  outputs = builtins.listToAttrs versions;

  genScriptForPush = let
    oVals = builtins.attrValues outputs;
    forVersions = f: builtins.map (x: f x) oVals;
    loadContainers = forVersions (x: ''
      docker load -i ${x.container}
    '');
    pushContainers = forVersions (x: ''
      docker push daml-cucumber:${x.version}
    '');
  in pkgs.writeShellScriptBin "docker-push-generated" (builtins.concatStringsSep "\n" (loadContainers ++ pushContainers));
in {
  container = outputs.daml-265.container;
  recurseForDerivations = true;
  pushScript = genScriptForPush;
} // outputs
