#!/usr/bin/env bash

cat << "EOF" >> ~/.bashrc
nvm() {
    unset -f nvm
    load_nvm
    nvm "$@"
}
node() {
    unset -f node
    load_nvm
    node "$@"
}
npm() {
    unset -f npm
    load_nvm
    npm "$@"
}
npx() {
    unset -f npx
    load_nvm
    npx "$@"
}
EOF
