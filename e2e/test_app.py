#!/usr/bin/env python3
"""End-to-end test: drives the real backend + the built frontend
bundle in a real (headless) browser via Playwright.

Expects two env vars:
  MEDIBOX_BACKEND_BIN  -- path to the medibox-backend executable
  MEDIBOX_FRONTEND_DIR -- directory containing index.html + index.js

No MIDI hardware is required or exercised: Medibox.Midi degrades to a
no-op when no ALSA sequencer is available, which is expected on CI
runners. This test only covers the WebSocket <-> SQLite <-> browser
path.
"""

import http.server
import os
import socket
import sqlite3
import subprocess
import sys
import tempfile
import threading
import time

from playwright.sync_api import sync_playwright


def free_port():
    with socket.socket(socket.AF_INET, socket.SOCK_STREAM) as s:
        s.bind(("127.0.0.1", 0))
        return s.getsockname()[1]


def wait_for_port(port, timeout=15):
    deadline = time.time() + timeout
    while time.time() < deadline:
        with socket.socket(socket.AF_INET, socket.SOCK_STREAM) as s:
            if s.connect_ex(("127.0.0.1", port)) == 0:
                return
        time.sleep(0.2)
    raise RuntimeError(f"nothing listening on port {port} after {timeout}s")


def serve_frontend(directory, port):
    handler = lambda *a, **kw: http.server.SimpleHTTPRequestHandler(
        *a, directory=directory, **kw
    )
    httpd = http.server.ThreadingHTTPServer(("127.0.0.1", port), handler)
    thread = threading.Thread(target=httpd.serve_forever, daemon=True)
    thread.start()
    return httpd


def query_db(db_path, sql, params=()):
    conn = sqlite3.connect(db_path)
    try:
        return conn.execute(sql, params).fetchall()
    finally:
        conn.close()


def main():
    backend_bin = os.environ["MEDIBOX_BACKEND_BIN"]
    frontend_dir = os.environ["MEDIBOX_FRONTEND_DIR"]

    with tempfile.TemporaryDirectory() as tmp:
        db_path = os.path.join(tmp, "medibox.db")
        backend_port = free_port()
        frontend_port = free_port()

        env = dict(os.environ)
        env["MEDIBOX_DB"] = db_path
        # backend hardcodes port 8080; run the real one, no MIDI hardware
        # is available here so Medibox.Midi runs in NoMidi mode.
        backend = subprocess.Popen([backend_bin], env=env)
        try:
            wait_for_port(8080)
            httpd = serve_frontend(frontend_dir, frontend_port)
            try:
                with sync_playwright() as p:
                    # --no-sandbox: CI runners commonly lack the user-namespace
                    # permissions Chromium's own sandbox needs.
                    browser = p.chromium.launch(args=["--no-sandbox"])
                    page = browser.new_page()
                    console_errors = []
                    page.on(
                        "console",
                        lambda msg: console_errors.append(msg.text)
                        if msg.type == "error" and "favicon" not in msg.text
                        else None,
                    )
                    page.goto(f"http://127.0.0.1:{frontend_port}/index.html")

                    # 1. Page loads with the auto-seeded Default song/track
                    # and 32 knobs, all showing value 0.
                    page.wait_for_selector("text=Medibox")
                    assert (
                        page.locator("button[aria-label^='Adjust CC 0,']").count()
                        == 1
                    )
                    assert (
                        page.locator("button[aria-label^='Adjust CC 31,']").count()
                        == 1
                    )
                    # 2. Create a song via the UI, confirm it round-trips to
                    # sqlite through the real backend.
                    page.fill('input[placeholder="New song"]', "E2E Song")
                    page.click("button:has-text('Create Song')")
                    page.wait_for_function(
                        "() => Array.from(document.querySelectorAll('option'))"
                        ".some(o => o.textContent === 'E2E Song')"
                    )
                    songs = query_db(db_path, "SELECT name FROM songs")
                    assert ("E2E Song",) in songs, songs

                    # 3. Create + select a track, drag a knob, confirm the
                    # value lands in sqlite.
                    page.fill('input[placeholder="New track"]', "E2E Track")
                    page.click("button:has-text('Create Track')")
                    page.wait_for_function(
                        "() => Array.from(document.querySelectorAll('option'))"
                        ".some(o => o.textContent.includes('E2E Track'))"
                    )
                    track_select = page.locator("select").nth(1)
                    track_select.select_option(label="0. E2E Track")
                    page.wait_for_timeout(300)

                    knob = page.locator("button[aria-label^='Adjust CC 3,']")
                    box = knob.bounding_box()
                    cx, cy = box["x"] + box["width"] / 2, box["y"] + box["height"] / 2
                    page.mouse.move(cx, cy)
                    page.mouse.down()
                    page.mouse.move(cx, cy - 60, steps=5)
                    page.mouse.up()
                    page.wait_for_function(
                        "() => document.querySelector(\"button[aria-label^='Adjust CC 3,']\")"
                        ".getAttribute('aria-label') !== 'Adjust CC 3, value 0'"
                    )

                    time.sleep(0.3)  # let the setParam websocket frame land
                    params = query_db(
                        db_path,
                        "SELECT value FROM parameters p "
                        "JOIN tracks t ON t.id = p.track_id "
                        "WHERE t.name = 'E2E Track' AND p.cc = 3",
                    )
                    assert params and params[0][0] > 0, params

                    # 4. Rename the current song via its click-to-edit label
                    # (pre-filled with the current name, since it's already
                    # named -- unlike a never-touched param's blank input).
                    song_label = page.locator("button:has-text('E2E Song')").first
                    song_label.click()
                    page.wait_for_function(
                        "() => document.activeElement.tagName === 'INPUT'"
                    )
                    page.keyboard.press("Control+A")
                    page.keyboard.type("Renamed Song")
                    page.keyboard.press("Enter")
                    page.wait_for_timeout(300)
                    songs = query_db(db_path, "SELECT name FROM songs")
                    assert ("Renamed Song",) in songs, songs

                    assert console_errors == [], console_errors

                    browser.close()
            finally:
                httpd.shutdown()
        finally:
            backend.terminate()
            try:
                backend.wait(timeout=5)
            except subprocess.TimeoutExpired:
                backend.kill()

    print("e2e: all assertions passed")


if __name__ == "__main__":
    try:
        main()
    except Exception as e:  # noqa: BLE001
        print(f"e2e FAILED: {e}", file=sys.stderr)
        raise
