#!/usr/bin/env bash

FILENAME=$HOME/Videos/Screencasts/$(date +%Y-%m-%d-%H-%M-%S).mp4

SCREEN_RES=$(xrandr --current | grep '\*' | awk '{print $1}')

case $1 in
    root)
        ffmpeg -f x11grab -framerate 30 -video_size "$SCREEN_RES" -i :0.0 -c:v libx264 -preset ultrafast -crf 18 "$FILENAME"
        ;;
    root-audio)
        ffmpeg -f x11grab -framerate 30 -video_size "$SCREEN_RES" -i :0.0 -f pulse -i default -c:v libx264 -preset ultrafast -crf 18 -c:a aac "$FILENAME"
        ;;
    region)
        read -r X Y H W <<< "$(slop -f "%X %Y %H %W")"
        ffmpeg -f x11grab -framerate 30 -video_size "${W}x${H}" -i ":0.0+${X},${Y}" -c:v libx264 -preset ultrafast -crf 18 "$FILENAME"
        ;;
    region-audio)
        read -r X Y H W <<< "$(slop -f "%X %Y %H %W")"
        ffmpeg -f x11grab -framerate 30 -video_size "${W}x${H}" -i ":0.0+${X},${Y}" -f pulse -i default -c:v libx264 -preset ultrafast -crf 18 -c:a aac "$FILENAME"
        ;;
    *)
        echo "usage: xscreenrecord.sh [root|root-audio|region|region-audio]"
        exit 1
        ;;
esac

notify-send "Screencast saved to $FILENAME"
