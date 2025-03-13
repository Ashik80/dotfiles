let g:mapleader = " "

let g:netrw_banner=0
let g:netrw_list_hide = '\(^\|\s\s\)\zs\.\S\+'
let g:netrw_localcopydircmd = 'cp -r'

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
set mouse=a
set wildmenu
set wildoptions=pum
set splitbelow
set splitright
set signcolumn=yes
set clipboard=unnamedplus
set hidden

" let &t_SI = "\e[6 q"
" let &t_SR = "\e[4 q"
" let &t_EI = "\e[2 q"

colorscheme retrobox

" General mappings
nnoremap <C-h> <C-w>h
nnoremap <C-j> <C-w>j
nnoremap <C-k> <C-w>k
nnoremap <C-l> <C-w>l
nnoremap ge :Ex<CR>
xnoremap > >gv
xnoremap < <gv
xnoremap J :move '>+1<CR>gv
xnoremap K :move '>-2<CR>gv
xnoremap < <gv

autocmd! BufEnter,BufWinEnter *.js,*.jsx,*.ts,*.tsx,*.json,*.rb,*.yml,*.html {
    set shiftwidth=2 tabstop=2
}

set grepprg=rg\ --no-heading\ --column
nnoremap <leader>fg :grep!<space>

" Fuzzy file finder
function! FuzzyFileFinder()
    let l:tmpfile = tempname()
    let l:cmd = "cat {}"
    if executable("batcat")
        let l:cmd = "batcat --theme=gruvbox-dark --style=numbers --color=always {}"
    endif
    execute "silent !rg --files | fzf --preview='".l:cmd."' | sed 's/$/:0:0/' > " . l:tmpfile
    set efm=%f:%l:%c
    silent execute 'cfile ' . l:tmpfile
    redraw!
endfunction
nnoremap <leader>ff :call FuzzyFileFinder()<CR>

" augroup AutoTagUpdate
"     autocmd!
"     autocmd BufWritePost * call UpdateTags()
" augroup END

" Update tag on save
function! UpdateTags()
    let tags_file = "tags"
    if filereadable(expand(tags_file))
        if executable("ctags")
            call job_start(["ctags", "-a", expand("%")])
        else
            echomsg "ctags not found! Install it to enable automatic tag updates." 
        endif
    endif
endfunction

" Turn of highlighting for tags file
augroup DisableSyntaxForTags
  autocmd!
  autocmd BufReadPre,BufNewFile tags setlocal syntax=OFF eventignore=all
augroup END
function! EnableSyntax()
    if &eventignore ==# 'all'
        set eventignore=
        execute ":e"
    endif
endfunction
nnoremap <leader><leader> :call EnableSyntax()<CR>

function! GitBlameSelection()
    let line_start = getpos("'<")[1]
    let line_end = getpos("'>")[1]
    execute "silent! terminal sh -c \"echo '\e[1;31mBlame results:\e[0m' && git-blame-colored ".expand("%")." -L".line_start.",".line_end."\""
    setlocal nobuflisted
endfunction
function! GitBlameFile()
    execute "silent! terminal sh -c \"echo '\e[1;31mBlame results:\e[0m' && git-blame-colored ".expand("%")."\""
    setlocal nobuflisted
endfunction
xnoremap gb :<C-u>call GitBlameSelection()<CR>
nnoremap gb :call GitBlameFile()<CR>

call plug#begin()

Plug 'Exafunction/codeium.vim'
Plug 'lilydjwg/colorizer'
Plug 'airblade/vim-gitgutter'
Plug 'yegappan/lsp'
Plug 'sheerun/vim-polyglot'
Plug 'sbdchd/neoformat'

call plug#end()

" Lsp settings
set tagfunc=lsp#lsp#TagFunc
let lspOpts = #{autoHighlightDiags: v:true}
autocmd User LspSetup call LspOptionsSet(lspOpts)

let lspServers = [#{
	\      name: 'typescriptlang',
	\      filetype: ['javascript', 'typescript', 'typescriptreact', 'javascriptreact'],
    \      path: 'typescript-language-server',
    \      args: ['--stdio']
	\ },
    \ #{
    \       name: 'gopls',
    \       filetype: ['go'],
    \       path: 'gopls',
    \       args: ['serve']
    \ },
    \ #{
    \       name: 'pyright',
    \       filetype: ['python'],
    \       path: 'pyright-langserver',
    \       args: ['--stdio'],
    \       workspaceConfig: #{
    \           python: #{
    \               pythonPath: '/home/ashik/.pyenv/shims/python3.9'
    \           }
    \       }
    \ }]

autocmd User LspSetup call LspAddServer(lspServers)

nnoremap <silent> K :LspHover<CR>
nnoremap <silent> grr :LspShowReferences<CR>
nnoremap <silent> grn :LspRename<CR>
nnoremap <silent> gca :LspCodeAction<CR>
nnoremap <silent> ]d :LspDiagNext<CR>
nnoremap <silent> [d :LspDiagPrev<CR>
nnoremap <silent> <leader>e :LspDiagCurrent<CR>

" Formatter settings
augroup fmt
  autocmd!
  autocmd BufWritePre * undojoin | Neoformat
augroup END
