let
  defNix = import ./default.nix;
in builtins.mapAttrs (k: v: {
  recurseForDerivations = true;
  daml-cucumber = v.hsBuild.daml-cucumber;
  daml-lib = v.damlLib;
  daml-test = v.damlTest;
  daml-example = v.damlExample;
  }) defNix.outputs
