runtime! commenter.vim

compiler ruster

command! MakeTags execute 'silent !ctags -R --exclude=.git --exclude=target .' | redraw!

vnoremap gc :<C-u>call CommentBlock()<CR>
nnoremap <leader>mt <cmd>MakeTags<CR>
vnoremap gc :<C-u>call CommentBlock("//")<CR>
nnoremap gcc :call CommentLine("//")<CR>
