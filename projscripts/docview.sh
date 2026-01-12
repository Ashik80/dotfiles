#!/usr/bin/env bash

if [ -z "$1" ]; then
    echo "Usage: docview.sh <filename:linenumber>"
    exit 1
fi

line="$1"
line="${line#\'}"
line="${line%\'}"
filename="${line%%:*}"
linenumber="${line#*:}"
linenumber="${linenumber%%:*}"

endingline=$((linenumber + 30))

sed -n "${linenumber},${endingline}p" "$filename"
