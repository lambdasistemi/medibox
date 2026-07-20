{
  description = "medibox - Behringer BCR2000 <-> browser bidirectional sync";

  nixConfig = {
    extra-substituters = [ "https://cache.iog.io" ];
    extra-trusted-public-keys =
      [ "hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ=" ];
  };

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils";
    purescript-overlay = {
      url = "github:paolino/purescript-overlay/fix/remove-nodePackages";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    mkSpagoDerivation = {
      url = "github:jeslie0/mkSpagoDerivation";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = { self, nixpkgs, flake-utils, purescript-overlay, mkSpagoDerivation }:
    flake-utils.lib.eachSystem [ "x86_64-linux" ] (system:
      let
        pkgs = import nixpkgs {
          inherit system;
          config.permittedInsecurePackages =
            [ "nodejs-20.20.2" "nodejs-slim-20.20.2" ];
          overlays = [
            purescript-overlay.overlays.default
            mkSpagoDerivation.overlays.default
          ];
        };
        project = import ./nix/project.nix { inherit pkgs; };
        inherit (project) hpkgs backend frontend shell;
        checks = import ./nix/checks.nix { inherit pkgs hpkgs backend; };
      in {
        packages = {
          inherit backend frontend;
          default = backend;
        };

        inherit checks;
        apps = import ./nix/apps.nix { inherit pkgs hpkgs checks; };

        devShells.default = shell;
      });
}
