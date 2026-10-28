#!/bin/bash
# tools/check-dmg.sh DMG -- that a disk image holds a browser that works.
#
# Mounts DMG read-only, checks what is in it and the application's signature,
# then starts the application from the mounted image -- as someone who has
# just downloaded it would -- pointed at a small Gopher server on this
# machine, with a data directory of its own so that nothing of the real
# user's is touched.  Passes if the page arrives, links and all, and the
# application is using the OpenSSL inside the image.  Used by CI and by hand.

set -euo pipefail

dmg="${1:?usage: tools/check-dmg.sh DMG}"
work="$(mktemp -d "${TMPDIR:-/tmp}/check-dmg.XXXXXX")"
mount="$work/mount"
app_pid=""
server_pid=""

cleanup() {
  for pid in "$app_pid" "$server_pid"; do
    if [ -n "$pid" ]; then
      kill "$pid" 2>/dev/null || true
      wait "$pid" 2>/dev/null || true
    fi
  done
  for attempt in 1 2 3 4 5; do
    hdiutil detach -quiet "$mount" 2>/dev/null && break
    sleep 1
  done
  hdiutil detach -quiet -force "$mount" 2>/dev/null || true
  if mount | grep -q "$work"; then
    echo "warning: $mount is still mounted; leaving $work" >&2
  else
    rm -rf "$work"
  fi
}
trap cleanup EXIT

mkdir -p "$mount" "$work/home"
hdiutil attach -readonly -nobrowse -noautoopen -mountpoint "$mount" "$dmg" > /dev/null
mount="$(cd "$mount" && pwd -P)"
app="$mount/Gopher.app"
test -d "$app" || { echo "error: no Gopher.app in $dmg" >&2; exit 1; }
test -L "$mount/Applications" || { echo "error: no link to /Applications in $dmg" >&2; exit 1; }
codesign --verify --deep --strict "$app"
echo "ok: the image holds a signed Gopher.app and a link to /Applications"

arch="$(uname -m)"
file "$app/Contents/MacOS/gopher-browser" | grep -q "$arch" \
  || { echo "error: the executable is not for $arch" >&2; exit 1; }
echo "ok: the executable is for $arch"

# A Gopher server that answers every request with the same small menu.
port="$(python3 -c 'import socket; s = socket.socket(); s.bind(("127.0.0.1", 0)); print(s.getsockname()[1])')"
python3 -I - "$port" > "$work/server.log" 2>&1 <<'SERVER' &
import socket, sys
port = int(sys.argv[1])
menu = ("iChecked from the disk image\t\terror.host\t1\r\n"
        "0A text file\t/hello.txt\t127.0.0.1\t%d\r\n"
        "1A menu\t/sub\t127.0.0.1\t%d\r\n.\r\n") % (port, port)
s = socket.socket()
s.setsockopt(socket.SOL_SOCKET, socket.SO_REUSEADDR, 1)
s.bind(("127.0.0.1", port))
s.listen(5)
while True:
    c, _ = s.accept()
    c.recv(4096)
    c.sendall(menu.encode())
    c.close()
SERVER
server_pid=$!

# env -u SBCL_HOME: a shell with Homebrew's sbcl on its path may export it,
# and the application would then load the wrong core.
env -u SBCL_HOME MACOS_APP_LOG="$work/app.log" GOPHER_BROWSER_HOME="$work/home" \
    GOPHER_BROWSER_URL="gopher://127.0.0.1:$port/" GOPHER_BROWSER_SELFTEST=20 \
    GOPHER_BROWSER_SNAPSHOT="$work/window.png" \
    "$app/Contents/MacOS/gopher-browser" > /dev/null 2>&1 &
app_pid=$!

# OpenSSL from inside the image, while the application is running.
for attempt in $(seq 1 30); do
  lsof -p "$app_pid" 2>/dev/null | grep -q libssl && break
  sleep 1
done
lsof -p "$app_pid" | grep libssl | grep -q "$mount/Gopher.app/Contents/Frameworks" \
  || { echo "error: the application is not using its own OpenSSL" >&2
       lsof -p "$app_pid" | grep -i 'libssl\|libcrypto' >&2 || true; exit 1; }
echo "ok: OpenSSL is the copy inside the image"

wait "$app_pid" || { echo "error: the application did not exit cleanly" >&2; cat "$work/app.log" >&2; exit 1; }
app_pid=""
grep -q "self test: showing gopher://127.0.0.1:$port/: .* (2 links)" "$work/app.log" \
  || { echo "error: the page did not arrive" >&2; cat "$work/app.log" >&2; exit 1; }
test -s "$work/window.png" || { echo "error: no picture of the window" >&2; exit 1; }
echo "ok: the browser showed the page, with its two links"
echo "$dmg is good"
