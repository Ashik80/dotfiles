runtime! commenter.vim

command! MakeTags execute 'silent !ctags -R --exclude=.git --exclude=venv .' | redraw!

let b:ale_fixers = ['black']
let b:ale_fix_on_save = 1
let b:ale_linters_ignore = ['mypy']

" generates the relative path of the file being imported from the project root
function! ImportRelativeFile()
    let l:splitted_source = split(@f, "/")
    let l:splitted_path = l:splitted_source[0:-2]
    let l:path = join(l:splitted_path, ".")
    let l:filename = split(l:splitted_source[-1], "\\.")[0]
    let l:relative_path = l:path.".".l:filename

    execute "norm a".l:relative_path
endfunction

compiler pyright

nnoremap <leader>pf <cmd>call ImportRelativeFile()<CR>
nnoremap <leader>mt <cmd>MakeTags<CR>
vnoremap gc :<C-u>call CommentBlock("#")<CR>
nnoremap gcc :call CommentLine("#")<CR>
