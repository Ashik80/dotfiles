runtime! commenter.vim

let b:ale_c_cc_options = '`pkg-config --cflags --libs gtk4`'
let b:ale_linters_ignore = ['clangd']

vnoremap gc :<C-u>call CommentBlock("//")<CR>
nnoremap gcc :call CommentLine("//")<CR>
