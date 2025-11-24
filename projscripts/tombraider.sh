#!/usr/bin/env bash

export GAME_DIR="/home/ashik/TombRaider/drive_c/RiseoftheTombRaider/"
export WINEPREFIX="/home/ashik/TombRaider"
export WINEARCH="win64"
export WINE="wine"

# wine performance settings
export WINEESYNC=1
export WINEFSYNC=1
export WINE_LARGE_ADDRESS_AWARE=1
export WINE_FULLSCREEN_FSR=1
export WINE_FULLSCREEN_FSR_STR=3

# shader cache
export MESA_GLTHREAD=1

cd "$GAME_DIR"
"$WINE" "ROTTR.exe"
