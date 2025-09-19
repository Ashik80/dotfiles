#!/usr/bin/env bash

~/dotfiles/projscripts/xsetkeyboard.sh

while true; do
    ~/dotfiles/projscripts/setdwmbar.sh

    sleep $((60 - $(date +%S)))
done &

feh --bg-fill ~/dotfiles/.config/hypr/wallpapers/3.png

dunst &

picom &

exec dwm
