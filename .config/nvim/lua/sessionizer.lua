-- Common session function
function start_job_and_connect(conn_command, abs_path)
    vim.fn.jobstart(conn_command, {
        detach = true
    })
    local function try_connect()
        if vim.fn.filereadable(abs_path) == 1 then
            vim.cmd("connect " .. abs_path)
        else
            vim.defer_fn(try_connect, 50)
        end
    end
    try_connect()
end

-- Built-in session chooser
function session_chooser()
    local buf, win = create_window()
    local command = "ls -a ~/*.sock | fzf --layout reverse"
    vim.cmd("startinsert")
    vim.fn.termopen({ "/bin/sh", "-c", command }, {
        on_exit = function(job_id, code, event)
            local lines = vim.api.nvim_buf_get_lines(buf, 0, -1, false)
            vim.api.nvim_win_close(win, true)
            vim.api.nvim_buf_delete(buf, { force = true })
            local selected = nil
            for _, line in ipairs(lines) do
                if line ~= "" then
                    selected = line
                    break
                end
            end
            if selected == nil then
                return
            end
            vim.cmd("connect " .. vim.fn.fnameescape(selected))
        end
    })
end
vim.keymap.set("n", "<leader>fs", session_chooser, { noremap = true, silent = true })

-- Built-in session creator
function session_creator()
    local buf, win = create_window()
    local gt_command = "find ~/src ~/Documents ~/projscripts ~/postgresql ~/dotfiles -type d \\( -name node_modules -o -name .git -o -name *cache* \\) -prune -o -type d -print | fzf --layout reverse"
    vim.cmd("startinsert")
    vim.fn.termopen({ "/bin/sh", "-c", gt_command }, {
        on_exit = function(job_id, code, event)
            local lines = vim.api.nvim_buf_get_lines(buf, 0, -1, false)
            vim.api.nvim_win_close(win, true)
            vim.api.nvim_buf_delete(buf, { force = true })
            local selected = nil
            for _, line in pairs(lines) do
                if line ~= "" then
                    selected = line
                end
            end
            if selected == nil then
                return
            end
            local basename = selected:match("([^/]+)$")
            local conn_command = string.format(
                'nvim --listen ~/%s.sock -c "cd %s" > /dev/null 2>&1 &',
                basename,
                selected
            )
            local abs_path = vim.fn.expand("~/" .. basename .. ".sock")
            start_job_and_connect(conn_command, abs_path)
        end
    })
end
vim.keymap.set("n", "<leader>fc", session_creator, { noremap = true, silent = true })

-- Project specific sessions
local default_sock = "~/bash.sock"
local default_path = "~"
local manzil_sock = "~/manzil.sock"
local manzil_path = "~/src/ManzilApp/manzil/"
local platform_be_sock = "~/platform-be.sock"
local platform_be_path = "~/src/ManzilApp/platform-be/"
local platform_fe_sock = "~/platform-fe.sock"
local platform_fe_path = "~/src/ManzilApp/platform-fe/"
local delserver_sock = "~/server.sock"
local delserver_path = "~/src/Deltagram/server/"

function create_project_session(sock_path, repo_path)
    local abs_path = vim.fn.expand(sock_path)
    if vim.fn.filereadable(abs_path) == 1 then
        vim.cmd("connect " .. abs_path)
    else
        local conn_command = string.format(
            'nvim --listen %s -c "cd %s" > /dev/null 2>&1 &',
            sock_path,
            repo_path
        )
        start_job_and_connect(conn_command, abs_path)
    end
end
vim.keymap.set("n", "<leader>fx", function() create_project_session(default_sock, default_path) end, { noremap = true, silent = true })
vim.keymap.set("n", "<leader>pm", function() create_project_session(manzil_sock, manzil_path) end, { noremap = true, silent = true })
vim.keymap.set("n", "<leader>pb", function() create_project_session(platform_be_sock, platform_be_path) end, { noremap = true, silent = true })
vim.keymap.set("n", "<leader>pf", function() create_project_session(platform_fe_sock, platform_fe_path) end, { noremap = true, silent = true })
vim.keymap.set("n", "<leader>ps", function() create_project_session(delserver_sock, delserver_path) end, { noremap = true, silent = true })
