set nocompatible
set hidden
let s:root = fnamemodify(expand('<sfile>'), ':p:h:h:h')
let s:config = readfile(s:root . '/.vimrc')
let s:start = index(s:config, '" Fuzzy file finder')
let s:end = index(s:config, '" Find files')
let s:source = tempname()
call writefile(s:config[s:start + 1 : s:end - 1], s:source)
execute 'source ' . fnameescape(s:source)
call delete(s:source)
let s:directory = tempname()
call mkdir(s:directory, 'p')
call writefile(['selected'], s:directory . '/file with spaces.txt')
execute 'cd ' . fnameescape(s:directory)
let $FZF_DEFAULT_OPTS = '--filter="file with spaces.txt"'
let s:window = win_getid()
call FuzzyFileFinder()
call assert_equal(2, winnr('$'), 'fzf opens in a split')
call assert_equal('terminal', &buftype, 'fzf runs in an embedded terminal')
let s:deadline = reltime()
while (winnr('$') > 1 || empty(expand('%'))) && reltimefloat(reltime(s:deadline)) < 5
    sleep 10m
endwhile
call assert_equal(s:window, win_getid(), 'returns to the original window')
call assert_equal(1, winnr('$'), 'terminal split is cleaned up')
call assert_equal('file with spaces.txt', expand('%:t'), 'opens the selected file')
let s:quickfix = getqflist()
let $FZF_DEFAULT_OPTS = '--filter=no-such-file'
call FuzzyFileFinder()
let s:deadline = reltime()
while winnr('$') > 1 && reltimefloat(reltime(s:deadline)) < 5
    sleep 10m
endwhile
call assert_equal(1, winnr('$'), 'empty selection closes the split')
call assert_equal('file with spaces.txt', expand('%:t'), 'empty selection keeps the current file')
call assert_equal(s:quickfix, getqflist(), 'empty selection keeps quickfix unchanged')
call delete(s:directory, 'rf')
if !empty(v:errors)
    for s:error in v:errors
        echomsg s:error
    endfor
    cquit
endif
qa!
