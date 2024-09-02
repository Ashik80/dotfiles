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

" TypeScript LSP-like
function! RunTSServer(job, status)
    set efm=%f(%l\\,%c):%m
    execute "cgetfile /tmp/output"
    cw
endfunction

autocmd! BufEnter,BufWritePost *.ts,*.tsx {
    job_start(["bash", "-c", "tsc -b -i > /tmp/output"], {
        \ exit_cb: "RunTSServer"
        \ })
}

" Python LSP-like
function! RunPyrightServer(job, status)
    set efm=%f:%l:%c\ %m
    execute "cgetfile /tmp/output"
    cw
endfunction

autocmd! BufEnter,BufWritePost *.py {
    b:cmd = 'pyright ' .. expand("%:h") .. ' | grep -E ":[0-9]+:[0-9]+" | sed "s/^\s*//" > /tmp/output'
    job_start(["bash", "-c", b:cmd], {
        \ exit_cb: "RunPyrightServer"
        \ })
}

autocmd! BufWritePost *.py {
    execute "!black %"
}
