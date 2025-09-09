vim9script

set wildmode=noselect:lastused,full
set wildmenu wildoptions=pum,fuzzy pumheight=15

cnoremap <Up> <C-U><Up>
cnoremap <Down> <C-U><Down>

nnoremap <space>ff :<C-u>find<space>

var files_cache: list<string> = []
augroup CmdComplete
    au!
    au CmdlineChanged : wildtrigger()
    au CmdlineEnter : files_cache = []
augroup END

def Find(cmd_arg: string, cmd_complete: bool): list<string>
    if empty(files_cache)
        var cmd = 'find . -type d \( -name node_modules -o -name .git -o -name dist -o -name *_cache -o -name __pycache__ -o -name android -o -name ios \) -prune -o -type f -print'
        files_cache = systemlist(cmd)
    endif
    if empty(cmd_arg)
        return files_cache
    else
        return files_cache->matchfuzzy(cmd_arg)
    endif
enddef

set findfunc=Find
