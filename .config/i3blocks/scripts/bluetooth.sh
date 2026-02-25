#!/usr/bin/env bash

BLUETOOTH_ICON="󰂯"
BLUETOOTH_STATUS=$(bluetoothctl show | awk '/Powered/ {print $2}')

if [ "$BLUETOOTH_STATUS" = "yes" ]; then
    BLUETOOTH="${BLUETOOTH_ICON}"
    if bluetoothctl info | grep 'Name' > /dev/null; then
        BLUETOOTH_DEVICE_NAME=$(bluetoothctl info | grep "Name" | sed 's/\s*Name:\s*//')
        BLUETOOTH="${BLUETOOTH_ICON} ${BLUETOOTH_DEVICE_NAME}"
    fi
else
    BLUETOOTH_ICON=""
fi

echo "$BLUETOOTH"
