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
set nobackup
set nowritebackup

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

" Macros
autocmd! BufEnter,BufWinEnter *.js,*.jsx,*.ts,*.tsx {
    call setreg('c', "yoconsole.log('\<c-r>\"', \<c-r>\");\<esc>")
    call setreg('l', "y}Oconsole.log('\<c-r>\"', \<c-r>\");\<esc>")
    call setreg('t', "itry {\<CR>} catch (error) {\<CR>}\<esc>Vkk=")
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
function! CocStatus()
    let l:status = coc#status()
    if l:status == ''
        return ''
    endif
    return '  | '.l:status
endfunction
set statusline=
set statusline+=%{GitBranch()}
set statusline+=%{FileName()}
set statusline+=%{CocStatus()}
set statusline+=\ %m
set statusline+=\ %r
set statusline+=%=
set statusline+=%y
set statusline+=\ \|\ %l,%c

" Plugins
call plug#begin()

Plug 'Exafunction/windsurf.vim'
Plug 'lilydjwg/colorizer'
Plug 'airblade/vim-gitgutter'
Plug 'neoclide/coc.nvim', {'branch': 'release'}
Plug 'sheerun/vim-polyglot'
Plug 'sainnhe/gruvbox-material'

call plug#end()

" Lsp settings
set tagfunc=CocTagFunc

function! CocHelperFocusFloat() abort
  let winid = coc#float#get_float_win()
  if winid > 0
    exec winid . "wincmd w"
  endif
endfunction

function! ShowDocumentation()
  if CocAction('hasProvider', 'hover')
    call CocActionAsync('doHover')
  else
    call feedkeys('K', 'in')
  endif
endfunction

nnoremap <silent> K :call ShowDocumentation()<CR>
nnoremap <silent> grr <Plug>(coc-references)
nnoremap <silent> grn <Plug>(coc-rename)
nnoremap <silent> gca <Plug>(coc-codeaction-cursor)
nnoremap <silent> ]d <Plug>(coc-diagnostic-next)
nnoremap <silent> [d <Plug>(coc-diagnostic-prev)
nnoremap <silent><expr> <leader>e coc#float#close_all()
nnoremap <silent> <leader>dl :CocDiagnostics<CR>
nnoremap <silent> <leader>sr :CocRestart<CR>
nnoremap <silent><expr> <C-f> coc#float#has_scroll() ? coc#float#scroll(1) : "\<C-f>"
nnoremap <silent><expr> <C-b> coc#float#has_scroll() ? coc#float#scroll(0) : "\<C-b>"
inoremap <silent><expr> <C-f> coc#float#has_scroll() ? "\<c-r>=coc#float#scroll(1)\<cr>" : "\<Right>"
inoremap <silent><expr> <C-b> coc#float#has_scroll() ? "\<c-r>=coc#float#scroll(0)\<cr>" : "\<Left>"
vnoremap <silent><expr> <C-f> coc#float#has_scroll() ? coc#float#scroll(1) : "\<C-f>"
vnoremap <silent><expr> <C-b> coc#float#has_scroll() ? coc#float#scroll(0) : "\<C-b>"
inoremap <silent><expr> <c-x><c-o> coc#refresh()

augroup coc-formatting
    autocmd!
    autocmd BufWritePre *.go,*.js,*.jsx,*.ts,*.tsx,*.py,*.json,*.html,*.css call CocAction('format')
augroup END

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

    autocmd ColorScheme * hi link CocErrorVirtualText DiagnosticError
    autocmd ColorScheme * hi link CocWarningVirtualText DiagnosticWarn
    autocmd ColorScheme * hi link CocInfoVirtualText DiagnosticInfo
    autocmd ColorScheme * hi link CocHintVirtualText DiagnosticHint
    autocmd ColorScheme * hi link CocErrorSign DiagnosticError
    autocmd ColorScheme * hi link CocWarningSign DiagnosticWarn
    autocmd ColorScheme * hi link CocInfoSign DiagnosticInfo
    autocmd ColorScheme * hi link CocHintSign DiagnosticHint
augroup END

let g:gruvbox_material_background = 'hard'
let g:gruvbox_material_better_performance = 1

colorscheme gruvbox-material
