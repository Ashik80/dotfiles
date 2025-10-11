#!/usr/bin/env bash

add_git_permissions() {
	git config core.sshCommand "ssh -i ~/.ssh/personal"
	git config user.name "Ashik80"
	git config user.email "ashikurrahman020995@gmail.com"
}

if [ $(ls -A ~/Documents | wc -l) -eq 0 ]; then
	cd ~/Documents
	GIT_SSH_COMMAND="ssh -i ~/.ssh/personal" git clone --depth 1 git@github.com:Ashik80/Documents.git .
	add_git_permissions
fi
if [ ! -d gpg-keys ]; then
	cd ~
	GIT_SSH_COMMAND="ssh -i ~/.ssh/personal" git clone --depth 1 git@github.com:Ashik80/gpg-keys.git
	cd gpg-keys
	add_git_permissions
fi
if [ ! -d .password-store ]; then
	cd ~
	GIT_SSH_COMMAND="ssh -i ~/.ssh/personal" git clone --depth 1 git@github.com:Ashik80/password-store.git .password-store
	cd .password-store
	add_git_permissions
fi
if [ ! -d dotfiles ]; then
	cd ~
	GIT_SSH_COMMAND="ssh -i ~/.ssh/personal" git clone --depth 1 git@github.com:Ashik80/dotfiles.git
	cd dotfiles
	add_git_permissions
fi
