"============== for default colorscheme ==============
hi LineNr ctermfg=248
hi MatchParen term=bold cterm=bold ctermfg=11 ctermbg=NONE guifg=#ffff00 guibg=NONE
hi Pmenu ctermbg=239 ctermfg=14
hi PmenuSel ctermbg=14 ctermfg=0
hi CursorLine term=NONE cterm=NONE ctermbg=237
hi CursorLineNr term=NONE cterm=NONE
hi SpellBad term=underline cterm=underline ctermfg=9 ctermbg=NONE
hi SpellCap term=underline cterm=underline ctermfg=12 ctermbg=NONE
hi SignColumn ctermbg=NONE

" statusline
hi CommandDisp ctermfg=0 ctermbg=11
hi GitDisp ctermfg=11 ctermbg=239
hi FileDisp ctermfg=white ctermbg=237
"=====================================================

"============== for everforest colorscheme ==============
if has('termguicolors')
    set termguicolors
endif

let g:everforest_background = 'hard'
let g:everforest_better_performance = 1
colorscheme everforest

" statusline
hi CommandDisp ctermfg=235 ctermbg=142 guifg=#272e33 guibg=#a7c080
hi GitDisp ctermfg=142 ctermbg=237 guifg=#a7c080 guibg=#374145
hi FileDisp ctermfg=245 ctermbg=236 guifg=#859289 guibg=#2e383c
