#!/usr/bin/env bash

if [ -d "/sys/class/backlight/intel_backlight" ]; then
    BRIGHTNESS_DIR="/sys/class/backlight/intel_backlight"
elif [ -d "/sys/class/backlight/amdgpu_bl1" ]; then
    BRIGHTNESS_DIR="/sys/class/backlight/amdgpu_bl1"
else
    echo "No backlight directory found"
    exit 1
fi

FILE="${BRIGHTNESS_DIR}/brightness"
MAX_FILE="${BRIGHTNESS_DIR}/max_brightness"

add_perm() {
    sudo chmod 666 "$FILE"
}

if [ ! -r "$FILE" ]; then
    add_perm
fi

if [ ! -w "$FILE" ]; then
    add_perm
fi

MIN=10
MAX="$(cat "$MAX_FILE")"
CURRENT=$(cat "$FILE")
NEW_VAL="$CURRENT"
CMD="$1"
PERCENT="$2"

VAL=$(( MAX * PERCENT / 100 ))

if [ "$CMD" = -inc ]; then
    NEW_VAL=$(( CURRENT + VAL ))
elif [ "$CMD" = -dec ]; then
    NEW_VAL=$(( CURRENT - VAL ))
elif [ "$CMD" = -set ]; then
    NEW_VAL=$(( VAL ))
fi

echo "Prev value: $CURRENT"
echo "New value: $NEW_VAL"

if [ $NEW_VAL -gt $MAX ]; then
    NEW_VAL=$MAX
elif [ $NEW_VAL -lt $MIN ]; then
    NEW_VAL=$MIN
fi

echo "$NEW_VAL" > "$FILE"
