runtime! commenter.vim

let b:ale_linters = ['clangd', 'clang']

vnoremap gc :<C-u>call CommentBlock("//")<CR>
nnoremap gcc :call CommentLine("//")<CR>
