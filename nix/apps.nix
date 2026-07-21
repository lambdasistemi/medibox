{ pkgs, hpkgs, checks }:
let
  format = pkgs.writeShellApplication {
    name = "format";
    runtimeInputs = [ hpkgs.fourmolu ];
    text = ''
      fourmolu -i backend/app backend/src backend/test
    '';
  };

  frontend-format = pkgs.writeShellApplication {
    name = "frontend-format";
    runtimeInputs = [ pkgs.purs-tidy-bin.purs-tidy-0_10_0 ];
    text = ''
      purs-tidy format-in-place 'frontend/src/**/*.purs'
    '';
  };

  runnable = { inherit (checks) lint frontend-lint; }
    // { inherit format frontend-format; };
in
builtins.mapAttrs
  (_: prog: {
    type = "app";
    program = pkgs.lib.getExe prog;
  })
  runnable
