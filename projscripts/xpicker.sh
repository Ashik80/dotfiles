#!/bin/bash

IMAGE="/tmp/screen.bmp"

xwininfo > /dev/null

RESULT=$(eval "$(xdotool getmouselocation --shell)" && import -window root "$IMAGE" && convert "$IMAGE" -crop "1x1+${X}+${Y}" txt: | grep -oP '#(?!\s)\w*')

rm -f "$IMAGE"

notify-send "$RESULT"

xclip -selection clipboard <<< "$RESULT"
