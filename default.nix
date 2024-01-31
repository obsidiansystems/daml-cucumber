let
  gitInfo = builtins.fetchGit { url = "file://${./.}"; };
  isDirty = gitInfo.shortRev == "0000000";
  rev = if isDirty then "dirty" else gitInfo.shortRev;
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
          substituteInPlace daml.yaml \
            --replace "2.6.5" ${version}
          daml build -o dist.dar
        '';
        installPhase = ''
          mkdir $out
          cp dist.dar $out/
        '';
      };
      damlTest = pkgs.stdenvNoCC.mkDerivation {
        name = "daml-cucumber-test";
        src = pkgs.lib.cleanSource ./.;
        buildInputs = [ damlSdk.jdk damlSdk.sdk ];
        buildPhase = ''
          cp ${damlLib}/dist.dar test/dist.dar
          substituteInPlace test/daml.yaml \
            --replace "2.6.5" ${version} \
            --replace "../daml/.daml/dist/daml-cucumber-0.1.0.0.dar" dist.dar
          ${hsBuild.daml-cucumber}/bin/daml-cucumber --generate-only --source ./test --features ./test/features.feature
          cd test && ${damlSdk.sdk}/bin/daml test > test-result
        '';
        installPhase = ''
          cat test-result > $out
        '';
      };
      container = mkContainer damlSdk hsBuild (version + "-${rev}");
      inherit version;
    };
  })
    (builtins.attrNames (builtins.readDir ((pkgs.hackGet ./nix/nix-daml-sdk) + "/versions")));

  mkContainer = nix-daml-sdk: hsBuild: version: pkgs.dockerTools.buildImage {
    name = "obsidiansys/daml-cucumber";
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
      docker push obsidiansys/daml-cucumber:${x.version}-${rev}
    '');
  in pkgs.writeShellScriptBin "docker-push-generated" (builtins.concatStringsSep "\n" (loadContainers ++ pushContainers));
in {
  inherit isDirty rev outputs;
  recurseForDerivations = true;
  pushScript = genScriptForPush;
}
