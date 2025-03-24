let g:mapleader = " "

let g:netrw_banner=0
let g:netrw_list_hide = '\(^\|\s\s\)\zs\.\S\+'
let g:netrw_localcopydircmd = 'cp -r'
let g:netrw_altfile = 1

filetype plugin indent on
syntax on
set bg=dark
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
set wildignore=**/.git/*,**/node_modules/**,**/dist/**,**/tmp/**
set path=**
set splitbelow
set splitright
set signcolumn=yes
set clipboard=unnamedplus
set hidden

" let &t_SI = "\e[6 q"
" let &t_SR = "\e[4 q"
" let &t_EI = "\e[2 q"

" General mappings
nnoremap <C-h> <C-w>h
nnoremap <C-j> <C-w>j
nnoremap <C-k> <C-w>k
nnoremap <C-l> <C-w>l
nnoremap ge :Ex<CR>
xnoremap > >gv
xnoremap < <gv
xnoremap J :move '>+1<CR>gv=gv
xnoremap K :move '<-2<CR>gv=gv
xnoremap < <gv

autocmd! BufEnter,BufWinEnter *.js,*.jsx,*.ts,*.tsx,*.json,*.rb,*.yml,*.html,*.css {
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

" Update tag on save
" augroup AutoTagUpdate
"     autocmd!
"     autocmd BufWritePost * call UpdateTags()
" augroup END
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

" Statusline
let g:git_branch = ''
function! UpdateGitBranch()
    let l:branch = system('git rev-parse --abbrev-ref HEAD 2>/dev/null')
    if !v:shell_error
        let g:git_branch = ' '.trim(l:branch).' | '
    else
        let g:git_branch = ''
    endif
endfunction
function! GitBranch()
    return g:git_branch
endfunction
augroup GitBranchAutoUpdate
    autocmd!
    autocmd BufEnter,FocusGained * call UpdateGitBranch()
augroup END
function! FileName()
    return expand('%') == '' ? '[No Name]' : expand('%:.')
endfunction
function! AleErrorCount()
    let l:counts = ale#statusline#Count(bufnr('%'))
    if l:counts.total == 0
        return ''
    endif
    let l:errors = l:counts.error > 0 ? l:counts.error . 'E ' : ''
    let l:warnings = l:counts.warning > 0 ? l:counts.warning . 'W ' : ''
    return '  | '.trim(l:errors . l:warnings)
endfunction
set statusline=
set statusline+=%{GitBranch()}
set statusline+=%{FileName()}
set statusline+=%{AleErrorCount()}
set statusline+=\ %m
set statusline+=\ %r
set statusline+=%=
set statusline+=%y
set statusline+=\ \|\ %l,%c

" Plugins
call plug#begin()

Plug 'Exafunction/codeium.vim'
Plug 'lilydjwg/colorizer'
Plug 'airblade/vim-gitgutter'
Plug 'dense-analysis/ale'
Plug 'sheerun/vim-polyglot'
Plug 'sainnhe/gruvbox-material'

call plug#end()

" Lsp settings
set completeopt=menu,menuone,popup,noselect,noinsert
set omnifunc=ale#completion#OmniFunc
let g:ale_completion_enabled = 1
let g:ale_fix_on_save = 1
" let g:ale_floating_preview = 1
let g:ale_linters = {
\   'javascript': ['tsserver', 'eslint'],
\   'typescript': ['tsserver', 'eslint'],
\   'javascriptreact': ['tsserver', 'eslint'],
\   'typescriptreact': ['tsserver', 'eslint'],
\   'python': ['pyright'],
\   'gohtmltmpl': ['templ'],
\}
let g:ale_fixers = {
\   'javascript': ['prettier', 'eslint'],
\   'typescript': ['prettier', 'eslint'],
\   'javascriptreact': ['prettier', 'eslint'],
\   'typescriptreact': ['prettier', 'eslint'],
\   'python': ['black'],
\   'go': ['gofmt'],
\}
call ale#linter#Define('gohtmltmpl', {
\   'name': 'templ',
\   'lsp': 'stdio',
\   'executable': 'templ',
\   'command': '%e lsp',
\   'project_root': getcwd(),
\})

nnoremap <silent> <C-]> :ALEGoToDefinition<CR>
nnoremap <silent> <C-w><C-]> :split<CR>:ALEGoToDefinition<CR>
nnoremap <silent> K :ALEHover<CR>
nnoremap <silent> grr :ALEFindReferences -quickfix<CR>:cw<CR>
nnoremap <silent> grn :ALERename<CR>
nnoremap <silent> gca :ALECodeAction<CR>
nnoremap <silent> ]d :ALENext<CR>
nnoremap <silent> [d :ALEPrevious<CR>
nnoremap <silent> <leader>e :ALEDetail<CR>
nnoremap <silent> <leader>dq :ALEPopulateQuickfix<CR>
nnoremap <silent> <leader>sr :ALEStopAllLSPs<CR>

" Formatter settings
augroup templft
    autocmd!
    autocmd BufWinEnter *.templ set filetype=gohtmltmpl
augroup END

augroup templFmt
    autocmd!
    autocmd BufWritePost *.templ silent! execute "!PATH=\"$PATH:$(go env GOPATH)/bin\" templ fmt <afile> >/dev/null 2>&1" | redraw!
augroup END

" Highlights
augroup Highlights
    autocmd!
    autocmd ColorScheme * hi DiagnosticError ctermfg=1 guifg=Red
    autocmd ColorScheme * hi DiagnosticWarn ctermfg=3 guifg=Orange
    autocmd ColorScheme * hi DiagnosticInfo ctermfg=4 guifg=LightBlue
    autocmd ColorScheme * hi DiagnosticHint ctermfg=7 guifg=LightGrey
    autocmd ColorScheme * hi DiagnosticUnderlineError cterm=underline gui=underline guisp=Red
    autocmd ColorScheme * hi DiagnosticUnderlineWarn cterm=underline gui=underline guisp=Orange
    autocmd ColorScheme * hi DiagnosticUnderlineInfo cterm=underline gui=underline guisp=LightBlue
    autocmd ColorScheme * hi DiagnosticUnderlineHint cterm=underline gui=underline guisp=LightGrey

    autocmd ColorScheme * hi link ALEError DiagnosticUnderlineError
    autocmd ColorScheme * hi link ALEWarning DiagnosticUnderlineWarn
    autocmd ColorScheme * hi link ALEInfo DiagnosticUnderlineInfo
    autocmd ColorScheme * hi link ALEVirtualTextError DiagnosticError
    autocmd ColorScheme * hi link ALEVirtualTextWarning DiagnosticWarn
    autocmd ColorScheme * hi link ALEVirtualTextInfo Diagnosticinfo
    autocmd ColorScheme * hi link ALEErrorSign DiagnosticError
    autocmd ColorScheme * hi link ALEWarningSign DiagnosticWarn
    autocmd ColorScheme * hi link ALEInfoSign DiagnosticInfo
augroup END

let g:gruvbox_material_background = 'hard'
let g:gruvbox_material_better_performance = 1

colorscheme gruvbox-material
