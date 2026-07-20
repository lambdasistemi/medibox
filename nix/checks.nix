{ pkgs, hpkgs, backend }:
{
  backend = backend;

  lint = pkgs.writeShellApplication {
    name = "lint";
    runtimeInputs = [ hpkgs.fourmolu hpkgs.hlint ];
    text = ''
      cd "${../. + "/"}"
      fourmolu -m check backend/app backend/src backend/test
      hlint backend/app backend/src backend/test
    '';
  };
}
