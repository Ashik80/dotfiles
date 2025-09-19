#!/usr/bin/env bash

TOUCHPAD_ID="$(xinput list | grep -i touchpad | grep -o "id=[0-9]*" | sed 's/id=//')"

NATURAL_SCROLLING_ID="$(xinput list-props "$TOUCHPAD_ID" | grep -i "natural scrolling" | grep -o "([0-9]*)" | grep -o "[0-9]*" | head -n 1)"

TAPPING_ID="$(xinput list-props "$TOUCHPAD_ID" | grep -i "tapping enabled" | grep -o "([0-9]*)" | grep -o "[0-9]*" | head -n 1)"

xinput set-prop "$TOUCHPAD_ID" "$NATURAL_SCROLLING_ID" 1
xinput set-prop "$TOUCHPAD_ID" "$TAPPING_ID" 1
