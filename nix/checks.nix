{ pkgs, hpkgs, backend, frontend }:
{
  backend = backend;
  frontend = frontend;

  lint = pkgs.writeShellApplication {
    name = "lint";
    runtimeInputs = [ hpkgs.fourmolu hpkgs.hlint ];
    text = ''
      cd "${../. + "/"}"
      fourmolu -m check backend/app backend/src backend/test
      hlint backend/app backend/src backend/test
    '';
  };

  frontend-lint = pkgs.writeShellApplication {
    name = "frontend-lint";
    runtimeInputs = [ pkgs.purs-tidy-bin.purs-tidy-0_10_0 ];
    text = ''
      cd "${../. + "/"}"
      purs-tidy check 'frontend/src/**/*.purs'
    '';
  };
}
