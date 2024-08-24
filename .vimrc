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

colorscheme habamax

autocmd! BufEnter *.js,*.jsx,*.ts,*.tsx,*.json {
    set shiftwidth=2 tabstop=2
}
