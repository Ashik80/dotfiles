#!/usr/bin/env bash

if [ -d "/sys/class/backlight/intel_backlight" ]; then
    BRIGHTNESS_DIR="/sys/class/backlight/intel_backlight"
else
    for d in /sys/class/backlight/amdgpu_bl*; do
        BRIGHTNESS_DIR="$d"
        break
    done
fi
BRIGHTNESS=$(cat "${BRIGHTNESS_DIR}/brightness")
MAX_BRIGHTNESS=$(cat "${BRIGHTNESS_DIR}/max_brightness")
BRIGHTNESS_PERCENTAGE=$(( BRIGHTNESS * 100 / MAX_BRIGHTNESS ))
BRIGHTNESS_ICON="󰃟"
BRIGHTNESS="${BRIGHTNESS_ICON} ${BRIGHTNESS_PERCENTAGE}%"

echo "$BRIGHTNESS"
