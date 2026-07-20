{ pkgs, hpkgs, checks }:
let
  format = pkgs.writeShellApplication {
    name = "format";
    runtimeInputs = [ hpkgs.fourmolu ];
    text = ''
      fourmolu -i backend/app backend/src backend/test
    '';
  };

  runnable = { inherit (checks) lint; } // { inherit format; };
in
builtins.mapAttrs
  (_: prog: {
    type = "app";
    program = pkgs.lib.getExe prog;
  })
  runnable
