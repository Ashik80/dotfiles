#!/usr/bin/env bash

RECORDING_ICON="󰕧"
if [ -f /tmp/xscreenrecord.pid ]; then
    RECORDING="${RECORDING_ICON} Rec"
else
    RECORDING=""
fi

echo "$RECORDING"
