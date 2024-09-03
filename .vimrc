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

let &t_SI = "\e[6 q"
let &t_SR = "\e[4 q"
let &t_EI = "\e[2 q"

colorscheme habamax

autocmd! BufEnter,BufWinEnter *.js,*.jsx,*.ts,*.tsx,*.json,*.rb,*.yml {
    set shiftwidth=2 tabstop=2
}

" Error checker server
function! StartServer(cmd, efm)
    let s:full_cmd = a:cmd . ' > /tmp/output'
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
    call StartServer('tsc -b -i', '%f(%l\\,%c):%m')
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
