#!/usr/bin/env bash

FILENAME=$HOME/Pictures/Screenshots/$(date +%Y-%m-%d-%H-%M-%S).png

case $1 in
	root)
        import -window root "$FILENAME"
		;;
	window)
        import -window "$(xdotool selectwindow)" "$FILENAME"
		;;
	region)
        REGION=$(slop) || exit 1
        import -window root -crop "$REGION" "$FILENAME"
		;;
    *)
        echo "usage: xscreenshot.sh [root|window|region]"
        exit 1
        ;;
esac

xclip -selection clipboard -t image/png "$FILENAME"

notify-send "Screenshot saved to $FILENAME"
