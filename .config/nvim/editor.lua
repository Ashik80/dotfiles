#!/usr/bin/env -S nvim -l
local ok, result = pcall(function()
    assert(#arg == 1, 'Expected one file to edit')
    assert(vim.env.NVIM and vim.env.NVIM ~= '', 'This editor must run inside a Neovim terminal')
    local channel = vim.fn.sockconnect('pipe', vim.env.NVIM, { rpc = true })
    assert(channel > 0, 'Could not connect to the parent Neovim')
    local win = vim.rpcrequest(channel, 'nvim_exec_lua',
        "return require('terminal_editor').open(...)", { vim.fn.fnamemodify(arg[1], ':p') })
    while true do
        local status = vim.rpcrequest(channel, 'nvim_exec_lua',
            "return require('terminal_editor').status(...)", { win })
        if status ~= -1 then
            vim.fn.chanclose(channel)
            return status
        end
        vim.wait(100)
    end
end)
if not ok then
    io.stderr:write('Neovim editor: ' .. tostring(result) .. '\n')
end
if not ok or result ~= 0 then
    vim.cmd('cquit 1')
end
