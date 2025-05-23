. ~/.bashrc

# prompt
export PS1='\e[0;33m\u\e[0m:\e[0;34m\w\e[0m$ '

# default editor
export EDITOR=vim
export VISUAL=$EDITOR

set -o vi

# aliases
alias l='ls -lh'
alias la='ls -lah'
alias editjournal="vim ~/Documents/Journal/$(date +%B,%Y)"
alias nv="nvim"
alias vi="vim"

# projscripts
export PATH="/home/ashik/projscripts:$PATH"

# local bin
export PATH="/home/ashik/.local/bin:$PATH"
