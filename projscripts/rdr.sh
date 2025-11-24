#!/usr/bin/env bash

export GAME_DIR="/home/ashik/Downloads/Games/Red Dead Redemption"
export WINEPREFIX="/home/ashik/RedDeadRedemption"
export WINEARCH="win64"
export WINE="/home/ashik/.local/share/lutris/runners/wine/wine-tkg-valve-exp-bleeding-experimental.bleeding.edge.10.0.261015.20251025/bin/wine"

# wine performance settings
export WINEESYNC=1
export WINEFSYNC=1
export WINE_LARGE_ADDRESS_AWARE=1
export WINE_FULLSCREEN_FSR=1
export WINEDEBUG="-all"
export WINEDLLOVERRIDES="d3d10core,d3d11,d3d12,d3d12core,d3d8,d3d9,d3dcompiler_33,d3dcompiler_34,d3dcompiler_35,d3dcompiler_36,d3dcompiler_37,d3dcompiler_38,d3dcompiler_39,d3dcompiler_40,d3dcompiler_41,d3dcompiler_42,d3dcompiler_43,d3dcompiler_46,d3dcompiler_47,d3dx10,d3dx10_33,d3dx10_34,d3dx10_35,d3dx10_36,d3dx10_37,d3dx10_38,d3dx10_39,d3dx10_40,d3dx10_41,d3dx10_42,d3dx10_43,d3dx11_42,d3dx11_43,d3dx9_24,d3dx9_25,d3dx9_26,d3dx9_27,d3dx9_28,d3dx9_29,d3dx9_30,d3dx9_31,d3dx9_32,d3dx9_33,d3dx9_34,d3dx9_35,d3dx9_36,d3dx9_37,d3dx9_38,d3dx9_39,d3dx9_40,d3dx9_41,d3dx9_42,d3dx9_43,dxgi,nvapi,nvapi64,nvofapi64=n;winemenubuilder="

# shader cache
export MESA_GLTHREAD=1
export __GL_SHADER_DISK_CACHE_PATH="$GAME_DIR"
export __GL_SHADER_DISK_CACHE=1

# wine cache
export WINE_MONO_CACHE_DIR="/home/ashik/.local/share/lutris/runners/wine/wine-tkg-valve-exp-bleeding-experimental.bleeding.edge.10.0.261015.20251025/mono"
export WINE_GECKO_CACHE_DIR="/home/ashik/.local/share/lutris/runners/wine/wine-tkg-valve-exp-bleeding-experimental.bleeding.edge.10.0.261015.20251025/gecko"

# dxvk settings
# export DXVK_ENABLE_NVAPI=1
# export DXVK_NVAPIHACK=0

cd "$GAME_DIR"
"$WINE" "$GAME_DIR/RDR.exe"
