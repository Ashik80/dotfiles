#!/usr/bin/env bash

FILENAME=$HOME/Videos/Screencasts/$(date +%Y-%m-%d-%H-%M-%S).mp4

SCREEN_RES=$(xrandr --current | grep '\*' | awk '{print $1}')

start () {
    case "$1" in
        root)
            ffmpeg -f x11grab -framerate 30 -video_size "$SCREEN_RES" -i :0.0 -c:v libx264 -preset ultrafast -crf 18 "$FILENAME" &
            echo "$!" > /tmp/xscreenrecord.pid
            ;;
        root-audio)
            ffmpeg -f x11grab -framerate 30 -video_size "$SCREEN_RES" -i :0.0 -f pulse -i default -c:v libx264 -preset ultrafast -crf 18 -c:a aac "$FILENAME" &
            echo "$!" > /tmp/xscreenrecord.pid
            ;;
        region)
            REGION=$(slop -D -f "%X %Y %H %W") || exit 1
            read -r X Y H W <<< "$REGION"
            ffmpeg -f x11grab -framerate 30 -video_size "${W}x${H}" -i ":0.0+${X},${Y}" -c:v libx264 -preset ultrafast -crf 18 "$FILENAME" &
            echo "$!" > /tmp/xscreenrecord.pid
            ;;
        region-audio)
            REGION=$(slop -D -f "%X %Y %H %W") || exit 1
            read -r X Y H W <<< "$REGION"
            ffmpeg -f x11grab -framerate 30 -video_size "${W}x${H}" -i ":0.0+${X},${Y}" -f pulse -i default -c:v libx264 -preset ultrafast -crf 18 -c:a aac "$FILENAME" &
            echo "$!" > /tmp/xscreenrecord.pid
            ;;
        *)
            echo "usage: xscreenrecord.sh [root|root-audio|region|region-audio]"
            exit 1
            ;;
    esac
}

if [ -f /tmp/xscreenrecord.pid ]; then
    kill -15 "$(cat /tmp/xscreenrecord.pid)"
    rm -f /tmp/xscreenrecord.pid
    notify-send "Screencast saved to $FILENAME"
    exit 0
fi

start "$1"
