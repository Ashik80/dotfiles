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
set wildignore=**/.git/*,**/node_modules/**,**/dist/**,**/tmp/**,**/ios/**,**cache**,**/android/**,**/.next/**
set path=**
set splitbelow
set splitright
set signcolumn=yes
set clipboard=unnamedplus
set hidden
set nobackup
set nowritebackup
set list
set listchars=tab:▸\ ,trail:·
set fillchars=eob:\ 

" Cursor change
let &t_SI = "\e[6 q"
let &t_SR = "\e[4 q"
let &t_EI = "\e[2 q"

" General mappings
nnoremap - :Ex<CR>
nnoremap ]q :cn<CR>
nnoremap [q :cp<CR>
xnoremap > >gv
xnoremap < <gv
xnoremap J :move '>+1<CR>gv=gv
xnoremap K :move '<-2<CR>gv=gv
nnoremap <leader>cp :let @+ = expand("%:.")<CR>

" Lazygit
if executable("lazygit")
    nnoremap <leader>lg :tabnew \| silent term ++curwin ++close lazygit<CR>
endif

" Indent
autocmd! BufEnter,BufWinEnter *.js,*.jsx,*.ts,*.tsx,*.json,*.rb,*.yml,*.html,*.css {
    set shiftwidth=2 tabstop=2
}

autocmd! FileType netrw set nocursorline

" Macros
autocmd! BufEnter,BufWinEnter *.js,*.jsx,*.ts,*.tsx {
    call setreg('c', "yoconsole.log('\<c-r>\"', \<c-r>\");\<esc>")
    call setreg('l', "y}Oconsole.log('\<c-r>\"', \<c-r>\");\<esc>")
    call setreg('t', "itry {\<CR>} catch (error) {\<CR>}\<esc>Vkk=")
    call setreg('p', "yothis.logger.debug('\<c-r>\"', \<c-r>\");\<esc>")
    call setreg('m', "y}Othis.logger.debug('\<c-r>\"', \<c-r>\");\<esc>")
}
autocmd! BufEnter,BufWinEnter *.py {
    call setreg('c', "yoprint('\<c-r>\"', \<c-r>\")\<esc>")
    call setreg('l', "y}Oprint('\<c-r>\"', \<c-r>\")\<esc>")
}

" Grepping
set grepprg=grep\ -Rn\ --exclude-dir={node_modules,.git,dist,*cache*,android,ios,.next}
nnoremap <leader>fg :grep!<space>

" Fuzzy file finder
function! FuzzyFileFinder()
    let l:tmpfile = tempname()
    let l:cmd = "cat {}"
    if executable("batcat")
        let l:cmd = "batcat --theme=ansi --style=numbers --color=always {}"
    endif
    execute "silent !rg --files | fzf --preview='".l:cmd."' | sed 's/$/:0:0/' > " . l:tmpfile
    set efm=%f:%l:%c
    silent execute 'cfile ' . l:tmpfile
    redraw!
endfunction
nnoremap <leader>fz :call FuzzyFileFinder()<CR>

" Find files
function! FindFilesToQf(pattern)
    let l:cmd = "rg --files | rg -i " . shellescape(a:pattern)
    let l:lines = systemlist(l:cmd)
    if len(l:lines) == 0
        return
    endif
    let l:items = []
    for l:line in l:lines
        let l:item = {'filename': l:line, 'lnum': 1, 'col': 1}
        call add(l:items, l:item)
    endfor
    call setqflist([], ' ', {'items': l:items, 'title': 'Find Files: ' . a:pattern})
    cw
endfunction
command! -nargs=1 FindFiles call FindFilesToQf(<f-args>)
nnoremap <leader>fq :FindFiles<space>

" Git blame
function! GitBlameSelection()
    let line_start = getpos("'<")[1]
    let line_end = getpos("'>")[1]
    execute "silent! terminal sh -c \"echo '\e[1;31mBlame results:\e[0m' && git-blame-colored ".shellescape(expand("%"))." -L".line_start.",".line_end."\""
    setlocal nobuflisted
endfunction
function! GitBlameFile()
    execute "silent! terminal sh -c \"echo '\e[1;31mBlame results:\e[0m' && git-blame-colored ".shellescape(expand("%"))."\""
    setlocal nobuflisted
endfunction
xnoremap gb :<C-u>call GitBlameSelection()<CR>
nnoremap gb :call GitBlameFile()<CR>

" Netrw copy file path
function! GetFilePath()
    return netrw#Call('NetrwFile', netrw#Call('NetrwGetWord'))
endfunction
function! CopyFile()
    if &filetype == 'netrw'
        let l:file = fnamemodify(GetFilePath(), ':.')
        let @+ = l:file
        echo "Copied file path to clipboard: " . l:file
    endif
endfunction
function! XdgOpenFile()
    if &filetype == 'netrw'
        let l:file = GetFilePath()
        call system('xdg-open ' . l:file)
    endif
endfunction
nnoremap cp :call CopyFile()<CR>
nnoremap go :call XdgOpenFile()<CR>

" Open a scratch buffer
function! OpenScratchBuffer()
    above new
    setlocal buftype=nofile
    setlocal bufhidden=hide
    setlocal noswapfile
    setlocal nobuflisted
endfunction
nnoremap <silent> <leader>o :call OpenScratchBuffer()<CR>

" Buffer to quickfix list
function! SelectionToQF()
  let startline = line("'<")
  let endline = line("'>")
  let lines = getline(startline, endline)
  if empty(lines)
    return
  endif
  let items = []
  for line in lines
    let m = matchlist(line, '\v^(\S+):(\d+):(\d+):(.*)')
    if !empty(m)
      call add(items, { 'filename': m[1], 'lnum': str2nr(m[2]), 'col': str2nr(m[3]), 'text': m[4] })
    else
      let m = matchlist(line, '\v^(\S+):(\d+):(.*)')
      if !empty(m)
        call add(items, { 'filename': m[1], 'lnum': str2nr(m[2]), 'col': 1, 'text': m[3] })
      endif
    endif
  endfor
  if !empty(items)
    call setqflist([], ' ', {'items': items, 'title': 'Selection to Quickfix'})
    cwindow
  endif
endfunction

xnoremap <silent> <leader>q :<C-u>call SelectionToQF()<CR>

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
    return l:status.' | '
endfunction
set statusline=
set statusline+=%{GitBranch()}
set statusline+=%{FileName()}
set statusline+=\ %m
set statusline+=\ %r
set statusline+=%=
if executable('node')
    set statusline+=%{CocStatus()}
endif
set statusline+=%y
set statusline+=\ \|\ %l,%c

" Execute scripts
augroup ExecuteScripts
  autocmd!
  autocmd FileType bash,sh call ExecuteScriptsSetup()
augroup END
function! ExecuteScriptsSetup() abort
  nnoremap <silent> <buffer> <leader>b :call RunShellBuffer()<CR>
endfunction
function! RunShellBuffer() abort
  %y
  call OpenScratchBuffer()
  put
  %!sh
endfunction

" Plugins
let g:plugins = [
    \ 'https://github.com/Exafunction/windsurf.vim',
    \ 'https://github.com/lilydjwg/colorizer',
    \ 'https://github.com/sheerun/vim-polyglot',
    \ 'https://github.com/neoclide/coc.nvim',
    \ 'https://github.com/junegunn/seoul256.vim',
    \ 'https://github.com/mhinz/vim-signify',
    \ 'https://github.com/menisadi/kanagawa.vim',
    \ ]

call plugger#setup(g:plugins)
nnoremap <silent> <leader>pu :call plugger#update()<CR>
nnoremap <silent> <leader>pc :call plugger#clean(g:plugins)<CR>

" Lsp settings
set tagfunc=CocTagFunc

let g:coc_enable_locationlist = 0
autocmd User CocLocationsChange CocList --normal location

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
nnoremap <silent> <leader>gd <Plug>(coc-definition)
nnoremap <silent> grr <Plug>(coc-references)
nnoremap <silent> grn <Plug>(coc-rename)
nnoremap <silent> gra <Plug>(coc-codeaction-cursor)
nnoremap <silent> gO :CocList -I symbols<CR>
nnoremap <silent> ]d <Plug>(coc-diagnostic-next)
nnoremap <silent> [d <Plug>(coc-diagnostic-prev)
inoremap <silent> <C-k> <C-r>=CocActionAsync('showSignatureHelp')<CR>
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

augroup coc-go-to-definition
    autocmd!
    autocmd BufEnter,BufWinEnter *.rb {
        nnoremap <silent> gd <Plug>(coc-definition)
    }
augroup END

augroup coc-formatting
    autocmd!
    autocmd BufWritePre *.go,*.js,*.jsx,*.ts,*.tsx,*.html,*.css call CocAction('format')
augroup END

" Formatter settings
function! FormatWithBlack()
    silent !black --quiet %
    redraw!
endfunction
augroup python-formatting
    autocmd!
    autocmd BufWritePost *.py call FormatWithBlack()
augroup END

" Git gutter
nnoremap <leader>gh :SignifyHunkDiff<CR>

" Highlights
" augroup Highlights
"     autocmd!
"     autocmd ColorScheme * hi DiagnosticError ctermfg=1 guifg=Red
"     autocmd ColorScheme * hi DiagnosticWarn ctermfg=3 guifg=Orange
"     autocmd ColorScheme * hi DiagnosticInfo ctermfg=4 guifg=LightBlue
"     autocmd ColorScheme * hi DiagnosticHint ctermfg=7 guifg=LightGrey
"     autocmd ColorScheme * hi DiagnosticUnderlineError cterm=underline gui=underline guisp=Red
"     autocmd ColorScheme * hi DiagnosticUnderlineWarn cterm=underline gui=underline guisp=Orange
"     autocmd ColorScheme * hi DiagnosticUnderlineInfo cterm=underline gui=underline guisp=LightBlue
"     autocmd ColorScheme * hi DiagnosticUnderlineHint cterm=underline gui=underline guisp=LightGrey
" 
"     autocmd ColorScheme * hi link CocErrorVirtualText DiagnosticError
"     autocmd ColorScheme * hi link CocWarningVirtualText DiagnosticWarn
"     autocmd ColorScheme * hi link CocInfoVirtualText DiagnosticInfo
"     autocmd ColorScheme * hi link CocHintVirtualText DiagnosticHint
"     autocmd ColorScheme * hi link CocErrorSign DiagnosticError
"     autocmd ColorScheme * hi link CocWarningSign DiagnosticWarn
"     autocmd ColorScheme * hi link CocInfoSign DiagnosticInfo
"     autocmd ColorScheme * hi link CocHintSign DiagnosticHint
" augroup END

colorscheme kanagawa

hi SignColumn ctermbg=NONE guibg=NONE
hi Normal ctermbg=NONE guibg=NONE
hi LineNr ctermbg=NONE guibg=NONE
hi DiffAdd ctermbg=NONE guibg=NONE
hi DiffChange ctermbg=NONE guibg=NONE
hi DiffDelete ctermbg=NONE guibg=NONE
hi VertSplit cterm=NONE
