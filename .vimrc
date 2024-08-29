syntax on
set number
set expandtab
set shiftwidth=4 tabstop=4
set autoindent smartindent
set ignorecase smartcase
set incsearch
set noswapfile
set laststatus=2
set ttimeoutlen=0
set backspace=indent,eol,start

let &t_SI = "\e[6 q"
let &t_SR = "\e[4 q"
let &t_EI = "\e[2 q"

colorscheme habamax

autocmd! BufEnter *.js,*.jsx,*.ts,*.tsx,*.json,*.rb,*.yml {
    set shiftwidth=2 tabstop=2
}
