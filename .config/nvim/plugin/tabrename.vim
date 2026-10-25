command! -nargs=1 TabRename let t:tab_name = <q-args> | redrawtabline

function! CustomTabline()
    let s = ''

    for i in range(tabpagenr('$'))
        let tabnr = i + 1
        let name = gettabvar(tabnr, 'tab_name', 'Tab ' . tabnr)

        let s .= tabnr == tabpagenr()
            \ ? '%#TabLineSel#'
            \ : '%#TabLine#'

        let s .= '%' . tabnr . 'T ' . substitute(name, '%', '%%', 'g') . ' '
    endfor

    return s . '%#TabLineFill#%T'
endfunction

set tabline=%!CustomTabline()
"set showtabline=2
