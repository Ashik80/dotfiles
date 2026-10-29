set nocompatible
set hidden
let s:root = fnamemodify(expand('<sfile>'), ':p:h:h:h')
let s:config = readfile(s:root . '/.vimrc')
let s:start = index(s:config, '" Emoji selector')
let s:end = index(s:config, '" Fuzzy file finder')
call assert_true(s:start >= 0 && s:end > s:start, 'emoji selector section exists')
if !empty(v:errors) | cquit | endif
let s:source = tempname()
call writefile(s:config[s:start + 1 : s:end - 1], s:source)
execute 'source ' . fnameescape(s:source)
call delete(s:source)
let s:home = $HOME
let $HOME = tempname() . ' home with spaces'
call mkdir($HOME . '/dotfiles/projscripts', 'p')
let s:script = $HOME . '/dotfiles/projscripts/emoji-selector'
call writefile(['#!/bin/sh', 'sleep 0.2', 'touch "$HOME/ran"', 'exit 1'], s:script)
call setfperm(s:script, 'rwx------')
let s:window = win_getid()
call EmojiSelector()
call assert_equal(2, winnr('$'), 'selector opens in a split')
call assert_equal(2, winnr(), 'selector opens below')
call assert_equal('terminal', &buftype, 'selector runs in a terminal')
let s:buffer = bufnr('%')
let s:deadline = reltime()
while winnr('$') > 1 && reltimefloat(reltime(s:deadline)) < 5
    sleep 10m
endwhile
call assert_equal(1, winnr('$'), 'terminal closes on exit')
call assert_false(bufexists(s:buffer), 'terminal buffer is wiped on exit')
call assert_equal(s:window, win_getid(), 'returns to original window')
call assert_true(filereadable($HOME . '/ran'), 'selector script ran')
call delete($HOME, 'rf')
let $HOME = s:home
if !empty(v:errors)
    for s:error in v:errors | echomsg s:error | endfor
    cquit
endif
qa!
