let g:mapleader = " "

filetype plugin indent on
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
set autoread
set termguicolors
set guifont=CommitMono\ 16

" let &t_SI = "\e[6 q"
" let &t_SR = "\e[4 q"
" let &t_EI = "\e[2 q"

colorscheme base16-classic-dark

autocmd! BufEnter,BufWinEnter *.js,*.jsx,*.ts,*.tsx,*.json,*.rb,*.yml {
    set shiftwidth=2 tabstop=2
}

" Fuzzy file finder
function! FuzzyFileFinder()
    execute "silent !fzf | sed 's/$/:0:0/' > /tmp/filefind"
    set efm=%f:%l:%c
    silent cfile /tmp/filefind
    redraw!
endfunction
nnoremap <leader>ff :call FuzzyFileFinder()<CR>

" Handmade compiler
function! Compile(cmd)
    let s:output = []
    function! GetCompiledErrors(channel, message)
        call add(s:output, a:message)
    endfunction

    function! PopulateQuickfixList(job, status)
        execute 'lgetexpr s:output'
        execute 'lw'
    endfunction

    call job_start(['bash', '-c', a:cmd], #{
        \ out_cb: 'GetCompiledErrors',
        \ err_cb: 'GetCompiledErrors',
        \ exit_cb: 'PopulateQuickfixList'
        \ })
endfunction

" Error checker server
function! StartServer(cmd, efm, is_stderr=v:false)
    let s:full_cmd = a:is_stderr ? a:cmd : a:cmd . ' > /tmp/output'

    execute 'set efm=' . a:efm

    function! SetCompiledErrors(job, status)
        execute 'cgetfile /tmp/output'
    endfunction

    call job_start(['bash', '-c', s:full_cmd], #{
        \ exit_cb: 'SetCompiledErrors'
        \ })
endfunction

" TypeScript settings
autocmd! BufEnter,BufWritePost *.ts,*.tsx {
    call StartServer('tsc -b -i', '%f(%l\\,%c):\ %m')
}

function! FormatWithPrettier()
    execute "silent !prettier -w %"
    execute "redraw!"
endfunction

autocmd! BufWritePost *.ts,*.tsx,*.js,*.jsx {
    call FormatWithPrettier()
}

" Python settings
function! RunPyrightServer()
    let s:cmd = 'pyright ' . expand("%") . ' | grep -E ":[0-9]+:[0-9]+" | sed "s/^\s*//"'
    call StartServer(s:cmd, '%f:%l:%c\ %m')
endfunction

autocmd! BufEnter,BufWinEnter *.py {
    call RunPyrightServer()
}

function! FormatWithBlack()
    execute "silent !black %"
    execute "redraw!"
endfunction

autocmd! BufWritePost *.py {
    call FormatWithBlack()
    call RunPyrightServer()
}

" Ruby settings
function! StartRubocopServer()
    let s:cmd = 'rubocop ' . expand("%")
    call StartServer(s:cmd, '%f:%l:%c:\ %m')
endfunction

autocmd! BufEnter,BufWritePost *.rb {
    call StartRubocopServer()
}

" Go settings
autocmd! BufEnter,BufWritePost *.go {
    call StartServer('go build 2> /tmp/output', '%f:%l:%c:\ %m', v:true)
}

function! FormatGo()
    execute "silent !gofmt -w %"
    execute "redraw!"
endfunction

autocmd! BufWritePost *.go {
    call FormatGo()
}

" Rust settings
autocmd! BufEnter,BufWritePost *.rs {
    call StartServer('cargo clippy 2>&1 | sed "s/  --> //" > /tmp/output', '%f:%l:%c', v:true)
}
