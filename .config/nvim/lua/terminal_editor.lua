local M = {}
local sessions = {}

function M.open(path)
    local origin = vim.api.nvim_get_current_win()
    local buf = vim.fn.bufadd(path)
    local was_loaded = vim.api.nvim_buf_is_loaded(buf)
    vim.fn.bufload(buf)
    if not was_loaded then
        vim.bo[buf].bufhidden = 'wipe'
    end
    local win = vim.api.nvim_open_win(buf, true, { split = 'below' })
    local session = {}
    sessions[win] = session

    local function restore()
        vim.schedule(function()
            if vim.api.nvim_win_is_valid(origin) and vim.api.nvim_get_current_win() == origin then
                if vim.bo[vim.api.nvim_win_get_buf(origin)].buftype == 'terminal' then
                    vim.cmd('startinsert')
                end
            end
        end)
    end
    vim.api.nvim_create_autocmd('WinClosed', {
        pattern = tostring(win),
        once = true,
        callback = function()
            if session.result == nil then
                session.result = vim.bo[buf].modified and 1 or 0
            end
            restore()
        end,
    })

    local function finish(save)
        if save then
            -- A failed write must leave the editor open and the caller waiting.
            vim.api.nvim_buf_call(buf, function() vim.cmd('write') end)
        end
        session.result = save and 0 or 1
        vim.api.nvim_win_close(win, true)
    end
    vim.api.nvim_buf_create_user_command(buf, 'EditorDone', function() finish(true) end, {})
    vim.api.nvim_buf_create_user_command(buf, 'EditorCancel', function() finish(false) end, {})
    vim.keymap.set('n', '<leader>w', '<cmd>EditorDone<CR>', { buffer = buf, desc = 'Save and return to terminal' })
    vim.keymap.set('n', '<leader>x', '<cmd>EditorCancel<CR>', { buffer = buf, desc = 'Cancel external editor' })
    vim.cmd('stopinsert')
    return win
end

function M.status(win)
    local session = assert(sessions[win], 'Unknown editor session')
    if session.result == nil then
        return -1
    end
    sessions[win] = nil
    return session.result
end

return M
