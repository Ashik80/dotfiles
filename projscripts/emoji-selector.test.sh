#!/usr/bin/env bash
set -eu

root=$(cd "$(dirname "$0")" && pwd)
tmp=$(mktemp -d)
trap 'rm -rf "$tmp"' EXIT
mkdir -p "$tmp/home/Documents" "$tmp/bin"
printf '😀 - smile\n' > "$tmp/home/Documents/emojis.txt"
printf '#!/bin/sh\ncat\n' > "$tmp/bin/fzf"
printf '#!/bin/sh\ncat > "$CAPTURE"\n' > "$tmp/bin/xclip"
printf '#!/bin/sh\ncat > "$CAPTURE"\n' > "$tmp/bin/wl-copy"
chmod +x "$tmp/bin/"*

for backend in xclip wl-copy; do
    rm -f "$tmp/capture"
    if [[ $backend == xclip ]]; then
        display='DISPLAY=:1 WAYLAND_DISPLAY='
    else
        display='DISPLAY= WAYLAND_DISPLAY=wayland-1'
    fi
    script -q -e -c "env HOME='$tmp/home' PATH='$tmp/bin:$PATH' CAPTURE='$tmp/capture' TMUX= $display '$root/emoji-selector'" /dev/null > "$tmp/terminal"
    printf '😀' > "$tmp/expected"
    cmp "$tmp/expected" "$tmp/capture"
    grep -q '52;c;8J+YgA==' "$tmp/terminal"
done
printf 'emoji-selector tests passed\n'
