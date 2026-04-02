#!/usr/bin/env bash

for i in {0..16}; do printf '%3d \e[48;5;%dm   \e[0m\n' "$i" "$i"; done
