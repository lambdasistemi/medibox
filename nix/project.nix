{ pkgs }:
let
  hpkgs = pkgs.haskell.packages.ghc9123;

  backend = hpkgs.callCabal2nix "medibox-backend" ../backend { };

  nodeModules = pkgs.importNpmLock.buildNodeModules {
    npmRoot = ../frontend;
    nodejs = pkgs.nodejs_20;
  };

  frontend = pkgs.mkSpagoDerivation {
    pname = "medibox-frontend";
    version = "0.1.0";
    src = ../frontend;
    spagoYaml = ../frontend/spago.yaml;
    spagoLock = ../frontend/spago.lock;
    nativeBuildInputs = [
      pkgs.purs
      pkgs.spago-unstable
      pkgs.esbuild
      pkgs.nodejs_20
    ];
    buildPhase = ''
      ln -s ${nodeModules}/node_modules node_modules
      esbuild src/bootstrap.js \
        --bundle \
        --outfile=dist/deps.js \
        --format=iife \
        --platform=browser \
        --minify
      spago bundle --offline --module Main
      cat dist/deps.js dist/index.js > dist/bundle.js
      mv dist/bundle.js dist/index.js
      rm dist/deps.js
    '';
    installPhase = ''
      mkdir -p $out
      cp dist/index.html $out/
      cp dist/index.js $out/
    '';
  };

  shell = hpkgs.shellFor {
    packages = p: [ backend ];
    nativeBuildInputs = [
      pkgs.cabal-install
      hpkgs.fourmolu
      hpkgs.hlint
      hpkgs.haskell-language-server
      pkgs.alsa-utils
      pkgs.sqlite
      pkgs.purs
      pkgs.spago-unstable
      pkgs.purs-tidy-bin.purs-tidy-0_10_0
      pkgs.purescript-language-server
      pkgs.esbuild
      pkgs.nodejs_20
      pkgs.just
    ];
  };
in
{
  inherit hpkgs backend frontend shell;
}
