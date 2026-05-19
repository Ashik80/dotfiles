function connect_to_pg_database()
    local buf, win = create_window()
    local command = "PGPASSWORD=postgres psql -U postgres -c 'select datname from pg_database' | head -n -2 | tail -n +3 | sed 's/^\\s\\+//' | fz -p 'Connect to database>'"
    vim.cmd("startinsert")
    vim.fn.termopen({'/bin/sh', '-c', command}, {
        on_exit = function()
            local lines = vim.api.nvim_buf_get_lines(buf, 0, -1, false)
            vim.api.nvim_win_close(win, true)
            vim.api.nvim_buf_delete(buf, { force = true })
            local selected = nil
            for _, line in pairs(lines) do
                if line ~= "" then
                    selected = line
                    break
                end
            end
            if selected == nil then
                return
            end
            vim.keymap.set('n', '<leader>b', string.format(':!PGPASSWORD=postgres psql -U postgres -d %s -f psql.sql -o out.sql<CR>', selected), { noremap = true, silent = true })
        end
    })
end
vim.api.nvim_create_user_command('ConnectToPGDatabase', connect_to_pg_database, {})
