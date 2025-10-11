#!/usr/bin/env bash

# shellcheck disable=SC1091
. "$HOME/dotfiles/projscripts/dmenu_helpers"

OPTIONS=("Wifi" "Volume" "Bluetooth" "Power" "Exit")

bluetooth_power() {
    local bt_power_options=("On" "Off" "Exit")
    local bt_power_choice

    bt_power_choice="$(dmenu_command "Bluetooth Power:" "${bt_power_options[@]}")"

    case $bt_power_choice in
        "On")
            bluetoothctl power on
            ;;
        "Off")
            bluetoothctl power off
            ;;
        "Exit")
            exit 0
            ;;
    esac
}

bluetooth_connect_to_device() {
    local bluetoothctl_devices
    local bt_device_names
    local bt_sel_device_name
    local bt_sel_device_id

    bluetoothctl_devices="$(bluetoothctl devices)"
    bt_device_names="$(echo "$bluetoothctl_devices" | grep 'Device' | sed 's/Device \(\w\+:\)\+\w\+\s//')"

    bt_sel_device_name="$(dmenu_command "Select Device:" "${bt_device_names[@]}")"

    if [ -z "$bt_sel_device_name" ]; then
        exit 0
    fi

    bt_sel_device_id="$(echo "$bluetoothctl_devices" | grep "$bt_sel_device_name" | awk '{print $2}')"

    notify-send "Connecting to $bt_sel_device_id - $bt_sel_device_name"

    bluetoothctl connect "$bt_sel_device_id"
}

power_options() {
    local power_options=("Power Off" "Reboot" "Exit")
    local choice
    local password

    choice="$(dmenu_command "Power Options:" "${power_options[@]}")"

    case $choice in
        "Power Off")
            if ! poweroff 2>/dev/null; then
                password="$(dmenu_input "Enter Password:")"
                echo "$password" | sudo -S poweroff
            fi
            ;;
        "Reboot")
            if ! reboot 2>/dev/null; then
                password="$(dmenu_input "Enter Password:")"
                echo "$password" | sudo -S reboot
            fi
            ;;
        "Exit")
            exit 0
            ;;
    esac
}

CHOICE="$(dmenu_command "Utility Launcher:" "${OPTIONS[@]}")"

case $CHOICE in
    "Wifi")
        if ! command -v impala &> /dev/null; then
            ~/dotfiles/projscripts/nm.sh
        else
            st -e impala
        fi
        ;;
    "Volume")
        st -e wiremix
        ;;
    "Bluetooth")
        options=("Connect" "Disconnect" "Scan" "Power")
        choice="$(dmenu_command "Bluetooth:" "${options[@]}")"

        case $choice in
            "Connect")
                bluetooth_connect_to_device
                ;;
            "Disconnect")
                bluetoothctl disconnect
                ;;
            "Scan")
                bluetoothctl scan on
                ;;
            "Power")
                bluetooth_power
                ;;
        esac
        ;;
    "Power")
        power_options
        ;;
    "Exit")
        exit 0
        ;;
esac
