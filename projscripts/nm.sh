#!/usr/bin/env bash

networks="$(nmcli -t -f BSSID,SSID,SIGNAL device wifi)"

dmenu_command() {
    local args=("$@")
    local title="${args[0]}"
    local list=("${args[@]:1}")
    local choice

    choice="$(printf '%s\n' "${list[@]}" | dmenu -fn "Menlo Nerd Font:size=15" -nb "#141415" -nf "#cdcdcd" -sb "#e8b589" -sf "#141415" -p "$title" -l 30 -b -i)"

    echo "$choice"
}

dmenu_input() {
    local title=$1
    local input

    input="$(echo | dmenu -fn "Menlo Nerd Font:size=15" -nb "#141415" -nf "#cdcdcd" -sb "#e8b589" -sf "#141415" -p "$title" -b)"

    echo "$input"
}

get_ssid_and_signal() {
    echo "$networks" | sed 's/\(.*\\:[A-Z0-9]*\):\(.*\):\([0-9]*\)$/\1|\2|\3%/' | sed 's/\\//g'
}

options=$(get_ssid_and_signal)

display=$(echo "$options" | column -t -s '|')

choice_line=$(dmenu_command "Select Network:" "$(echo "$display" | nl -w2 -s'. ')")

line_num="$(echo "$choice_line" | awk '{print $1}' | sed 's/\.//')"

choice="$(echo "$options" | sed -n "${line_num}p")"

name="$(echo "$choice" | awk -F '|' '{print $2}')"
bssid="$(echo "$choice" | awk -F '|' '{print $1}')"

notify-send "Connecting to $name"

output="$(nmcli d wifi connect "$bssid" 2>&1)"

if echo "$output" | grep -q "successfully activated"; then
    notify-send "Connected to $name"
elif echo "$output" | grep -qi "secrets were required"; then
    password="$(dmenu_input "Password for $name")"
    output="$(nmcli d wifi connect "$bssid" password "$password" 2>&1)"
    notify-send "$output"
else
    notify-send "Failed to connect to $name"
fi
