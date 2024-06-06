runtime! commenter.vim

vnoremap gc :<C-u>call CommentBlock("#")<CR>
nnoremap gcc :call CommentLine("#")<CR>
