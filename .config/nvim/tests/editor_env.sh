#!/usr/bin/env bash
set -euo pipefail
snippet=${1:-"$HOME/Documents/bashrc-snippet"}

bash --noprofile --norc -c '
    bind() { :; }
    export NVIM=/tmp/test-neovim.sock
    export EDITOR="$2" VISUAL="$2" GIT_EDITOR="$2"
    source "$1"
    [[ "$EDITOR" == "$2" && "$VISUAL" == "$2" && "$GIT_EDITOR" == "$2" ]] || {
        echo "Shell startup overwrote the Neovim editor bridge" >&2
        exit 1
    }
' _ "$snippet" "$HOME/.config/nvim/editor.lua"

bash --noprofile --norc -c '
    bind() { :; }
    unset NVIM
    export EDITOR=other-editor VISUAL=other-editor
    source "$1"
    [[ "$EDITOR" == nvim && "$VISUAL" == nvim ]] || {
        echo "Normal shell editor defaults changed" >&2
        exit 1
    }
' _ "$snippet"
printf '%s\n' 'editor environment tests passed'
