#!/usr/bin/env bash

DIR=$HOME/src/ManzilApp

add_git_permissions() {
	git config core.sshCommand "ssh -i ~/.ssh/office"
	git config user.name "ashikur-rahman-sazim"
	git config user.email "ashikur.rahman@sazim.io"
}

if [ ! -d "$DIR" ]; then
    mkdir -p "$DIR"
    cd $DIR
fi

if [ ! -d "$DIR"/manzil ]; then
    cd "$DIR"
    GIT_SSH_COMMAND="ssh -i ~/.ssh/office" git clone --depth 1 git@github.com:Manzilapp/manzil.git
    cd manzil
    add_git_permissions
fi

if [ ! -d "$DIR"/platform-be ]; then
    cd "$DIR"
    GIT_SSH_COMMAND="ssh -i ~/.ssh/office" git clone --depth 1 git@github.com:Manzilapp/platform-be.git
    cd platform-be
    add_git_permissions
fi

if [ ! -d "$DIR"/platform-fe ]; then
    cd "$DIR"
    GIT_SSH_COMMAND="ssh -i ~/.ssh/office" git clone --depth 1 git@github.com:Manzilapp/platform-fe.git
    cd platform-fe
    add_git_permissions
fi

if [ ! -d "$DIR"/AdminPortal ]; then
    cd "$DIR"
    GIT_SSH_COMMAND="ssh -i ~/.ssh/office" git clone --depth 1 git@github.com:Manzilapp/AdminPortal.git
    cd AdminPortal
    add_git_permissions
fi
