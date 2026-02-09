#!/usr/bin/env bash

add_git_permissions() {
	git config core.sshCommand "ssh -i ~/.ssh/personal"
	git config user.name "Ashik80"
	git config user.email "ashikurrahman020995@gmail.com"
}

if [ ! -d $HOME/src/Deltagram ]; then
    cd $HOME/src

    GIT_SSH_COMMAND="ssh -i ~/.ssh/personal" git clone --recurse-submodules git@github.com:The-Deltagram/Deltagram.git

    ln -s $HOME/Documents/Deltagram/deltagram-env $HOME/src/Deltagram/.env

    cd $HOME/src/Deltagram
    add_git_permissions
fi

if [ -d $HOME/src/Deltagram/server ] && [ ! -f $HOME/src/Deltagram/server/.env ]; then
    ln -s $HOME/Documents/Deltagram/server-env $HOME/src/Deltagram/server/.env
    cd $HOME/src/Deltagram/server
    add_git_permissions
fi

if [ -d $HOME/src/Deltagram/client ] && [ ! -f $HOME/src/Deltagram/client/.env ]; then
    ln $HOME/Documents/Deltagram/client-development-env $HOME/src/Deltagram/client/.env.development
    cd $HOME/src/Deltagram/client
    add_git_permissions
fi

if [ -d $HOME/src/Deltagram/admin ] && [ ! -f $HOME/src/Deltagram/admin/.env ]; then
    ln $HOME/Documents/Deltagram/client-development-env $HOME/src/Deltagram/admin/.env.development
    cd $HOME/src/Deltagram/admin
    add_git_permissions
fi
