#!/usr/bin/env bash

# shellcheck disable=SC1091
. "$HOME/dotfiles/projscripts/rofi_helpers"

networks="$(nmcli -t -f BSSID,SSID,SIGNAL device wifi)"

get_ssid_and_signal() {
    echo "$networks" | sed 's/\(.*\\:[A-Z0-9]*\):\(.*\):\([0-9]*\)$/\1|\2|\3%/' | sed 's/\\//g'
}

options=$(get_ssid_and_signal)

display=$(echo "$options" | column -t -s '|')

choice_line=$(rofi_command "Select Network" "$(echo "$display" | nl -w2 -s'. ')")

if [ -z "$choice_line" ]; then
    exit 0
fi

line_num="$(echo "$choice_line" | awk '{print $1}' | sed 's/\.//')"

choice="$(echo "$options" | sed -n "${line_num}p")"

name="$(echo "$choice" | awk -F '|' '{print $2}')"
bssid="$(echo "$choice" | awk -F '|' '{print $1}')"

notify-send "Connecting to $name"

output="$(nmcli d wifi connect "$bssid" 2>&1)"

if echo "$output" | grep -q "successfully activated"; then
    notify-send "Connected to $name"
elif echo "$output" | grep -qi "secrets were required"; then
    password="$(rofi_input "Password for $name")"
    output="$(nmcli d wifi connect "$bssid" password "$password" 2>&1)"
    notify-send "$output"
else
    notify-send "Failed to connect to $name"
fi
