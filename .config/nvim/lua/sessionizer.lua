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
    local command = "ls -a ~/*.sock | grep -Eo '[A-Za-z0-9_-]*.sock' | sed 's/.sock//' | fuzzy -p 'Sessions>'"
    local home_path = vim.fn.expand("~")
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
            vim.cmd("connect " .. home_path .. "/" .. vim.fn.fnameescape(selected) .. ".sock")
        end
    })
end
vim.keymap.set("n", "<leader>fs", session_chooser, { noremap = true, silent = true })

-- Built-in session creator
function session_creator()
    local outputfile = vim.fn.tempname()
    local buf, win = create_window()
    local gt_command = string.format("find ~/src ~/Documents ~/projscripts ~/postgresql ~/dotfiles -type d \\( -name node_modules -o -name .git -o -name *cache* \\) -prune -o -type d -print | fuzzy -p 'Create session>' > %s", outputfile)
    vim.cmd("startinsert")
    vim.fn.termopen({ "/bin/sh", "-c", gt_command }, {
        on_exit = function(job_id, code, event)
            -- local lines = vim.api.nvim_buf_get_lines(buf, 0, -1, false)
            local lines = vim.fn.readfile(outputfile)
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
            os.remove(outputfile)
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
local default = { sock_path = "~/bash.sock", repo_path = "~" }
local manzil = { sock_path = "~/manzil.sock", repo_path = "~/src/ManzilApp/manzil/" }
local platform_be = { sock_path = "~/platform-be.sock", repo_path = "~/src/ManzilApp/platform-be/" }
local platform_fe = { sock_path = "~/platform-fe.sock", repo_path = "~/src/ManzilApp/platform-fe/" }
local delserver = { sock_path = "~/server.sock", repo_path = "~/src/Deltagram/server/" }
local dotfiles = { sock_path = "~/dotfiles.sock", repo_path = "~/dotfiles/" }
local documents = { sock_path = "~/Documents.sock", repo_path = "~/Documents" }

--@param project { sock_path = string, repo_path = string }
function create_project_session(project)
    local sock_path = project.sock_path
    local repo_path = project.repo_path
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
vim.keymap.set("n", "<leader>lx", function() create_project_session(default) end, { noremap = true, silent = true })
vim.keymap.set("n", "<leader>lm", function() create_project_session(manzil) end, { noremap = true, silent = true })
vim.keymap.set("n", "<leader>lb", function() create_project_session(platform_be) end, { noremap = true, silent = true })
vim.keymap.set("n", "<leader>lf", function() create_project_session(platform_fe) end, { noremap = true, silent = true })
vim.keymap.set("n", "<leader>ls", function() create_project_session(delserver) end, { noremap = true, silent = true })
vim.keymap.set("n", "<leader>ld", function() create_project_session(dotfiles) end, { noremap = true, silent = true })
vim.keymap.set("n", "<leader>lD", function() create_project_session(documents) end, { noremap = true, silent = true })
vim.keymap.set("n", "<leader>fk", function()
    vim.fn.jobstart("pkill -f 'nvim --listen'", { detach = true })
end, { noremap = true, silent = true })
