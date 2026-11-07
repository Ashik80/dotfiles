function! ToggleCheckmark()
    let l:linecontent = getline('.')
    if l:linecontent =~# '- \[ \] '
        let l:newline = substitute(l:linecontent, '- \[ \] ', '- \[x\] ', '')
    elseif l:linecontent =~# '- \[x\] '
        let l:newline = substitute(l:linecontent, '- \[x\] ', '- \[ \] ', '')
    endif
    call setline('.', l:newline)
endfunction

function! ToggleChecklist()
    let l:linecontent = getline('.')
    if l:linecontent =~# '- \[.\] '
        let l:newline = substitute(l:linecontent, '- \[.\] ', '', '')
    elseif l:linecontent =~# '- '
        let l:newline = substitute(l:linecontent, '- ', '- \[ \] ', '')
    else
        let l:newline = substitute(l:linecontent, '\(\w\)', '- \[ \] \1', '')
    endif
    call setline('.', l:newline)
endfunction

nnoremap <leader>tc :call ToggleCheckmark()<CR>
nnoremap <leader>tt :call ToggleChecklist()<CR>
