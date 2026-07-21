# medibox

Keeps a Behringer BCR2000 control surface in sync with a browser:
turning a knob on the device updates a SQLite-backed model of
songs/tracks/parameters and pushes the change to every connected
browser over a WebSocket; editing a value from the browser pushes the
MIDI Control Change back out to the device.

## Development

```
nix develop
just build-backend
just test-backend
just dev-backend   # needs ALSA + a BCR2000 connected (or aseqdump/amidi for testing)

just build-frontend
just bundle-frontend  # writes frontend/dist/index.js
```

Serve `frontend/dist/` with any static file server while `medibox-backend`
is running on `localhost:8080` to use the UI.

See `justfile` for the full recipe list. `nix/` holds the flake split
(`project.nix`/`checks.nix`/`apps.nix`); `backend/` is the Haskell
sync server, `frontend/` is the PureScript/Halogen UI, `legacy/` is
the original 2015 CLI sequencer + gtk2hs knob panel kept for
reference.

## License

BSD-3-Clause
