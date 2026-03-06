#!/usr/bin/env bash

VOL_MUTE_STATUS="$(wpctl get-volume @DEFAULT_AUDIO_SINK@)"
if echo "$VOL_MUTE_STATUS" | grep -q "\[MUTED\]"; then
    VOLUME_ICON="󰝟"
else
    VOLUME_ICON="󰕾"
fi

MIC_MUTE_STATUS="$(wpctl get-volume @DEFAULT_AUDIO_SOURCE@)"
if echo "$MIC_MUTE_STATUS" | grep -q "\[MUTED\]"; then
    MIC_ICON="󰍭"
else
    MIC_ICON="󰍬"
fi

VOLUME_LEVEL="$(echo "$(wpctl get-volume @DEFAULT_AUDIO_SINK@ | grep -o "[0-9]\+\(\.[0-9]\+\)") * 100 / 1" | bc)"
MIC_LEVEL="$(echo "$(wpctl get-volume @DEFAULT_AUDIO_SOURCE@ | grep -o "[0-9]\+\(\.[0-9]\+\)") * 100 / 1" | bc)"
VOLUME="${VOLUME_ICON} ${VOLUME_LEVEL} ${MIC_ICON} ${MIC_LEVEL}"

echo "$VOLUME"
