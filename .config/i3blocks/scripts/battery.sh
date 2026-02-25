#!/usr/bin/env bash

BATTERY_CAPACITY=$(cat /sys/class/power_supply/BAT*/capacity)
BATTERY_STATUS=$(cat /sys/class/power_supply/BAT*/status)

if [ "$BATTERY_STATUS" = "Charging" ]; then
    BATTERY_ICON="󰂄"
elif [ "$BATTERY_STATUS" = "Discharging" ] || [ "$BATTERY_STATUS" = "Not charging" ]; then
    if [ "$BATTERY_CAPACITY" -lt 20 ]; then
        BATTERY_ICON="󰁻"
    elif [ "$BATTERY_CAPACITY" -lt 40 ]; then
        BATTERY_ICON="󰁽"
    elif [ "$BATTERY_CAPACITY" -lt 60 ]; then
        BATTERY_ICON="󰁿"
    elif [ "$BATTERY_CAPACITY" -lt 80 ]; then
        BATTERY_ICON="󰂁"
    elif [ "$BATTERY_CAPACITY" -lt 90 ]; then
        BATTERY_ICON="󰂂"
    else
        BATTERY_ICON="󰁹"
    fi
elif [ "$BATTERY_STATUS" = "Full" ]; then
    BATTERY_ICON="󰁹"
fi

BATTERY="${BATTERY_ICON} ${BATTERY_CAPACITY}%"

echo "$BATTERY"
