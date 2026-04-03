-- Finding
files_cache = {}
function FindFunc(cmdarg, cmdline)
    if #files_cache == 0 then
        local cmd = string.format(
            [[find . -type d \( -name node_modules -o -name .git -o -name dist -o -name *cache* -o -name android -o -name ios -o -name .next \) -prune -o -type f -print]]
        )
        files_cache = vim.fn.systemlist(cmd)
    end
    if cmdarg == "" then
        return files_cache
    else
        return vim.fn.matchfuzzy(files_cache, cmdarg)
    end
end
vim.o.findfunc = "v:lua.FindFunc"
augroup('CmdComplete', { clear = true })
autocmd('CmdlineChanged', {
    group = 'CmdComplete',
    pattern = { ':' },
    callback = function()
        vim.fn.wildtrigger()
    end
})
autocmd('CmdlineEnter', {
    group = 'CmdComplete',
    pattern = { ':' },
    callback = function()
        files_cache = {}
    end
})
