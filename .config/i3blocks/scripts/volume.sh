#!/usr/bin/env bash

VOL_MUTE_STATUS="$(pactl get-sink-mute @DEFAULT_SINK@)"
if [ "$VOL_MUTE_STATUS" = "Mute: yes" ]; then
    VOLUME_ICON="󰝟"
else
    VOLUME_ICON="󰕾"
fi

MIC_MUTE_STATUS="$(pactl get-source-mute @DEFAULT_SOURCE@)"
if [ "$MIC_MUTE_STATUS" = "Mute: yes" ]; then
    MIC_ICON="󰍭"
else
    MIC_ICON="󰍬"
fi

VOLUME_LEVEL="$(pactl get-sink-volume @DEFAULT_SINK@ | grep -o '[0-9]*%' | head -n 1)"
MIC_LEVEL="$(pactl get-source-volume @DEFAULT_SOURCE@ | grep -o '[0-9]*%' | head -n 1)"
VOLUME="${VOLUME_ICON} ${VOLUME_LEVEL} ${MIC_ICON} ${MIC_LEVEL}"

echo "$VOLUME"
