#!/usr/bin/env bash

FILENAME=$HOME/Videos/Screencasts/$(date +%Y-%m-%d-%H-%M-%S).mp4

SCREEN_RES=$(xrandr --current | grep '\*' | awk '{print $1}' | head -n 1)

start () {
    case "$1" in
        root)
            ffmpeg -f x11grab -framerate 30 -video_size "$SCREEN_RES" -i "$DISPLAY" -c:v libx264 -preset ultrafast -crf 18 -pix_fmt yuv420p "$FILENAME" &
            echo "$!" > /tmp/xscreenrecord.pid
            ;;
        root-audio)
            ffmpeg -f x11grab -framerate 30 -video_size "$SCREEN_RES" -i "$DISPLAY" -f pulse -i default -c:v libx264 -preset ultrafast -crf 18 -pix_fmt yuv420p -c:a aac "$FILENAME" &
            echo "$!" > /tmp/xscreenrecord.pid
            ;;
        region)
            REGION=$(slop -D -f "%X %Y %H %W") || exit 1
            read -r X Y H W <<< "$REGION"
            # to make the region divisible by 2
            (( W = W / 2 * 2 ))
            (( H = H / 2 * 2 ))
            ffmpeg -f x11grab -framerate 30 -video_size "${W}x${H}" -i "${DISPLAY}+${X},${Y}" -c:v libx264 -preset ultrafast -crf 18 -pix_fmt yuv420p "$FILENAME" &
            echo "$!" > /tmp/xscreenrecord.pid
            ;;
        region-audio)
            REGION=$(slop -D -f "%X %Y %H %W") || exit 1
            read -r X Y H W <<< "$REGION"
            # to make the region divisible by 2
            (( W = W / 2 * 2 ))
            (( H = H / 2 * 2 ))
            ffmpeg -f x11grab -framerate 30 -video_size "${W}x${H}" -i "${DISPLAY}+${X},${Y}" -f pulse -i default -c:v libx264 -preset ultrafast -crf 18 -pix_fmt yuv420p -c:a aac "$FILENAME" &
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
