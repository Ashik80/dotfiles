#!/usr/bin/env bash

~/dotfiles/projscripts/xsetkeyboard.sh

~/dotfiles/projscripts/settouchpad.sh

~/dotfiles/projscripts/xdisablescreensaver.sh

while true; do
    ~/dotfiles/projscripts/setdwmbar.sh

    sleep $((60 - $(date +%S)))
done &

feh --bg-fill ~/dotfiles/.config/hypr/wallpapers/glowing-purple-cloud.jpg

dunst &

picom &

exec dwm
