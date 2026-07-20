# shellcheck shell=bash

set unstable := true

# List available recipes
default:
    @just --list

# Build the backend
build-backend:
    cd backend && cabal build -O0

# Build the frontend
build-frontend:
    cd frontend && spago build

# Run the backend server (needs ALSA + a BCR2000 connected, or aseqdump/amidi for testing)
dev-backend:
    cd backend && cabal run -O0 medibox-backend

# Rebuild the frontend on change
dev-frontend:
    cd frontend && spago build --watch

# Bundle the frontend into frontend/dist
bundle-frontend:
    #!/usr/bin/env bash
    set -euo pipefail
    cd frontend
    esbuild src/bootstrap.js --bundle --outfile=dist/deps.js --format=iife --platform=browser --minify
    spago bundle --module Main --outfile dist/index.js
    cat dist/deps.js dist/index.js > dist/bundle.js
    mv dist/bundle.js dist/index.js
    rm dist/deps.js

# Format both sides
format:
    cd backend && fourmolu -i app src
    cd frontend && purs-tidy format-in-place 'src/**/*.purs'

# Lint both sides
lint:
    cd backend && hlint app src
    cd frontend && purs-tidy check 'src/**/*.purs'

# Full local CI
ci: build-backend build-frontend lint
