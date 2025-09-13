#!/usr/bin/env bash

# Volume
VOLUME="󰕾 $(pactl get-sink-volume "$(pactl get-default-sink)" | grep -o '[0-9]*%' | head -n 1)"

# Date
DATE=" $(date "+%e %b %Y, %I:%M %P")"

# Wifi
NETWORK_NAME=$(iwctl station wlan0 show | grep 'Connected network' | sed 's/\s*Connected network\s*//' | sed 's/\s*$//')
RSSI=$(iwctl station wlan0 show | awk '/\sRSSI/ {print $2}')
SIGNAL_PERCENTAGE=$(( (RSSI + 100) * 2 ))

if [ "$SIGNAL_PERCENTAGE" -lt 0 ]; then
    SIGNAL_PERCENTAGE=0
elif [ "$SIGNAL_PERCENTAGE" -gt 100 ]; then
    SIGNAL_PERCENTAGE=100
fi

if [ "$SIGNAL_PERCENTAGE" -eq 0 ]; then
    SIGNAL_ICON="󰤭"
elif [ "$SIGNAL_PERCENTAGE" -lt 20 ]; then
    SIGNAL_ICON="󰤬"
elif [ "$SIGNAL_PERCENTAGE" -lt 40 ]; then
    SIGNAL_ICON="󰤟"
elif [ "$SIGNAL_PERCENTAGE" -lt 60 ]; then
    SIGNAL_ICON="󰤢"
elif [ "$SIGNAL_PERCENTAGE" -lt 80 ]; then
    SIGNAL_ICON="󰤥"
elif [ "$SIGNAL_PERCENTAGE" -lt 100 ]; then
    SIGNAL_ICON="󰤨"
elif [ "$SIGNAL_PERCENTAGE" -eq 100 ]; then
    SIGNAL_ICON="󰤨"
else
    SIGNAL_ICON="󰤭"
fi

SIGNAL="${SIGNAL_ICON} ${NETWORK_NAME}"

# Battery
BATTERY_CAPACITY=$(cat /sys/class/power_supply/BAT1/capacity)
BATTERY_STATUS=$(cat /sys/class/power_supply/BAT1/status)

if [ "$BATTERY_STATUS" = "Charging" ]; then
    BATTERY_ICON="󰂄"
elif [ "$BATTERY_STATUS" = "Discharging" ]; then
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

# Bluetooth
BLUETOOTH_ICON="󰂯"
BLUETOOTH_STATUS=$(bluetoothctl show | awk '/Powered/ {print $2}')

if [ "$BLUETOOTH_STATUS" = "yes" ]; then
    BLUETOOTH=" ${BLUETOOTH_ICON} |"
    # bluetoothctl info | grep 'Name' > /dev/null
    if bluetoothctl info | grep 'Name' > /dev/null; then
        BLUETOOTH_DEVICE_NAME=$(bluetoothctl info | grep "Name" | sed 's/\s*Name:\s*//')
        BLUETOOTH=" ${BLUETOOTH_ICON} ${BLUETOOTH_DEVICE_NAME} |"
    fi
else
    BLUETOOTH_ICON=""
fi

# Set dwm status
xsetroot -name "${BLUETOOTH} ${DATE} | ${VOLUME} | ${SIGNAL} | ${BATTERY}"
