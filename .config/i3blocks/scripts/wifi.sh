#!/usr/bin/env bash

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

echo "$SIGNAL"
