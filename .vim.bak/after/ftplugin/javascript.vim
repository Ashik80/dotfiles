runtime! commenter.vim

setlocal shiftwidth=2 tabstop=2

command! EslintFixAll execute 'silent !eslint_d --fix %' | redraw!
command! PrettierFormat execute 'silent !prettier -w %' | redraw!
command! MakeTags execute 'silent !ctags -R --exclude=.git --exclude=node_modules --exclude=.build --exclude=dist --exclude=.next .' | redraw!

" generates the relative path of the file being imported from the current file
function! ImportRelativeFile()
    let l:splitted_source = split(@f, "/")
    let l:source_filename = l:splitted_source[-1]
    let l:source_path = l:splitted_source[0:-2]

    let l:current_file = expand("%:.")
    let l:splitted_dest = split(l:current_file, "/")

    let l:matches_until = 0
    while l:matches_until < len(l:splitted_dest)
        if l:splitted_source[l:matches_until] != l:splitted_dest[l:matches_until]
            break
        endif
        let l:matches_until += 1
    endwhile

    let l:rest_source_path = l:splitted_source[l:matches_until:]
    let l:rest_dest_path = l:splitted_dest[l:matches_until:-2]

    let l:relative_path = ""
    for path in l:rest_dest_path
        let l:relative_path = l:relative_path . "../"
    endfor
    if len(l:rest_dest_path) == 0
        let l:relative_path = "./"
    endif

    let l:source_relative_path = l:relative_path . join(l:rest_source_path, "/")
    execute "norm a".l:source_relative_path
endfunction

nnoremap <leader>fe <cmd>EslintFixAll<CR>
nnoremap <leader>fp <cmd>PrettierFormat<CR>
nnoremap <leader>pf <cmd>call ImportRelativeFile()<CR>
nnoremap <leader>mt <cmd>MakeTags<CR>
vnoremap gc :<C-u>call CommentBlock("//")<CR>
nnoremap gcc :call CommentLine("//")<CR>
