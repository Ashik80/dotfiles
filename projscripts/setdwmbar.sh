#!/usr/bin/env bash

# Volume
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

# Date
DATE=" $(date "+%e %b %Y, %a %I:%M %P")"

# Wifi
if ! command -v nmcli &> /dev/null; then
    NETWORK_NAME=$(iwctl station wlan0 show | grep 'Connected network' | sed 's/\s*Connected network\s*//' | sed 's/\s*$//')
    RSSI=$(iwctl station wlan0 show | awk '/RSSI/ {print $2}' | head -n 1)
    SIGNAL_PERCENTAGE=$(( (RSSI + 100) * 2 ))

    if [ "$SIGNAL_PERCENTAGE" -lt 0 ]; then
        SIGNAL_PERCENTAGE=0
    elif [ "$SIGNAL_PERCENTAGE" -gt 100 ]; then
        SIGNAL_PERCENTAGE=100
    fi
else
    NETWORK=$(nmcli -t -f ACTIVE,SSID,SIGNAL d wifi | grep ^yes)
    NETWORK_NAME=$(echo "$NETWORK" | awk -F':' '{print $2}')
    SIGNAL_PERCENTAGE=$(echo "$NETWORK" | awk -F':' '{print $3}')
fi

if [ -n "$NETWORK_NAME" ]; then
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
else
    SIGNAL_ICON="󰤭"
fi

SIGNAL="${SIGNAL_ICON} ${NETWORK_NAME}"

# Battery
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

# Bluetooth
BLUETOOTH_ICON="󰂯"
BLUETOOTH_STATUS=$(bluetoothctl show | awk '/Powered/ {print $2}')

if [ "$BLUETOOTH_STATUS" = "yes" ]; then
    BLUETOOTH=" ${BLUETOOTH_ICON} |"
    if bluetoothctl info | grep 'Name' > /dev/null; then
        BLUETOOTH_DEVICE_NAME=$(bluetoothctl info | grep "Name" | sed 's/\s*Name:\s*//')
        BLUETOOTH=" ${BLUETOOTH_ICON} ${BLUETOOTH_DEVICE_NAME} |"
    fi
else
    BLUETOOTH_ICON=""
fi

# Brightness
if [ -d "/sys/class/backlight/intel_backlight" ]; then
    BRIGHTNESS_DIR="/sys/class/backlight/intel_backlight"
elif [ -d "/sys/class/backlight/amdgpu_bl1" ]; then
    BRIGHTNESS_DIR="/sys/class/backlight/amdgpu_bl1"
fi
BRIGHTNESS=$(cat "${BRIGHTNESS_DIR}/brightness")
MAX_BRIGHTNESS=$(cat "${BRIGHTNESS_DIR}/max_brightness")
BRIGHTNESS_PERCENTAGE=$(( BRIGHTNESS * 100 / MAX_BRIGHTNESS ))
BRIGHTNESS_ICON="󰃟"
BRIGHTNESS="${BRIGHTNESS_ICON} ${BRIGHTNESS_PERCENTAGE}%"

# Recording
RECORDING_ICON="󰕧"
if [ -f /tmp/xscreenrecord.pid ]; then
    RECORDING=" ${RECORDING_ICON} Rec |"
else
    RECORDING=""
fi

# Set dwm status
xsetroot -name "${BLUETOOTH}${RECORDING} ${DATE} | ${BRIGHTNESS} | ${VOLUME} | ${SIGNAL} | ${BATTERY}"
