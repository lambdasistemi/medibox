{ pkgs, hpkgs, checks, backend, frontend }:
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

  e2e = pkgs.writeShellApplication {
    name = "e2e";
    runtimeInputs = [ (pkgs.python3.withPackages (ps: [ ps.playwright ])) ];
    text = ''
      cd "${../. + "/"}"
      export PLAYWRIGHT_BROWSERS_PATH=${pkgs.playwright-driver.browsers}
      export PLAYWRIGHT_SKIP_BROWSER_DOWNLOAD=1
      export MEDIBOX_BACKEND_BIN=${backend}/bin/medibox-backend
      export MEDIBOX_FRONTEND_DIR=${frontend}
      python3 e2e/test_app.py
    '';
  };

  runnable = { inherit (checks) lint frontend-lint; }
    // { inherit format frontend-format e2e; };
in
builtins.mapAttrs
  (_: prog: {
    type = "app";
    program = pkgs.lib.getExe prog;
  })
  runnable
