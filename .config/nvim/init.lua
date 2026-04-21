vim.g.mapleader = " "

vim.o.number = true
vim.o.expandtab = true
vim.o.shiftwidth = 4
vim.o.tabstop = 4
vim.o.smartindent = true
vim.o.ignorecase = true
vim.o.smartcase = true
vim.o.swapfile = false
vim.o.termguicolors = true
vim.o.wildignore = "**/.git/*,**/node_modules/**,**/dist/**,**/tmp/**,**/ios/**,**cache**,**/android/**,**/.next/**"
vim.o.path = "**"
vim.o.hlsearch = false
vim.o.splitbelow = true
vim.o.splitright = true
vim.o.signcolumn = "yes"
vim.o.clipboard = "unnamedplus"
vim.o.writebackup = false
vim.o.winborder = "solid"
vim.o.guicursor = ""
vim.o.listchars = "tab:▸ ,trail:·"
vim.o.list = true
vim.o.fillchars = "eob: "
vim.o.wildmenu = true
vim.o.wildoptions = "pum"

local augroup = vim.api.nvim_create_augroup
local autocmd = vim.api.nvim_create_autocmd

-- Mappings
-- vim.keymap.set('n', '-', '<cmd>Ex<CR>')
vim.keymap.set('n', '-', '<cmd>VimExplorer<CR>')
vim.keymap.set({'v', 'x'}, '>', '>gv')
vim.keymap.set({'v', 'x'}, '<', '<gv')
vim.keymap.set({'v', 'x'}, 'J', ":move '>+1<CR>gv=gv")
vim.keymap.set({'v', 'x'}, 'K', ":move '<-2<CR>gv=gv")
vim.keymap.set('n', '<leader>cp', ':let @+ = expand("%:.")<CR>')
vim.keymap.set('t', '<C-w>N', '<C-\\><C-n>')
vim.keymap.set('t', '<Esc><Esc>', '<C-\\><C-n>')
vim.keymap.set('t', '<C-w>w', '<C-\\><C-n><C-w>w')
vim.keymap.set('t', '<C-w><C-w>', '<C-\\><C-n><C-w>w')
vim.keymap.set('c', '<Down>', function()
  return vim.fn.pumvisible() == 1 and '<C-n>' or '<Down>'
end, { expr = true })
vim.keymap.set('c', '<Up>', function()
  return vim.fn.pumvisible() == 1 and '<C-p>' or '<Up>'
end, { expr = true })

-- Grepping
vim.o.grepprg = "grep -Rn --exclude-dir={node_modules,.git,dist,*cache*,android,ios,.next}"
vim.o.grepformat = "%f:%l:%m"
vim.keymap.set('n', '<leader>fg', ':grep!<space>')

-- Clean no name buffers
vim.api.nvim_create_user_command("CleanNoNameBuffers", function()
    vim.cmd [[bufdo if bufname('%') == '' && line('.') == 1 && getline('.') == '' | bdelete | endif]]
end, {})

-- Indent
augroup('JSIndent', { clear = true })
autocmd('FileType', {
    group = 'JSIndent',
    pattern = { 'javascript', 'typescript', 'javascriptreact', 'typescriptreact', "json", "css" },
    callback = function()
        vim.bo.tabstop = 2
        vim.bo.shiftwidth = 2
    end
})

-- Macros
local paste_from_reg = vim.api.nvim_replace_termcodes('<C-r>"', true, true, true)
local esc = vim.api.nvim_replace_termcodes('<ESC>', true, true, true)
local enter = vim.api.nvim_replace_termcodes('<CR>', true, true, true)
augroup('JSMacros', { clear = true })
autocmd('FileType', {
    group = 'JSMacros',
    pattern = { 'javascript', 'typescript', 'javascriptreact', 'typescriptreact' },
    callback = function()
        vim.fn.setreg("c", "yoconsole.log('" .. paste_from_reg .. "', " .. paste_from_reg .. ");" .. esc)
        vim.fn.setreg("l", "y}Oconsole.log('" .. paste_from_reg .. "', " .. paste_from_reg .. ");" .. esc)
        vim.fn.setreg("t", "itry {" .. enter .. "} catch (error) {" .. enter .. "}" .. esc .. "Vkk=")
    end
})
augroup('PythonMacros', { clear = true })
autocmd('FileType', {
    group = 'PythonMacros',
    pattern = { 'python' },
    callback = function()
        vim.fn.setreg('c', "yoprint('" .. paste_from_reg .. "', " .. paste_from_reg .. ")" ..esc)
        vim.fn.setreg('l', "y}Oprint('" .. paste_from_reg .. "', " .. paste_from_reg .. ")" ..esc)
    end
})

-- Create floating window
function create_window()
    local buf = vim.api.nvim_create_buf(false, true)
    local ui = vim.api.nvim_list_uis()[1]
    local width = math.floor(ui.width * 0.8)
    local height = math.floor(ui.height * 0.8)
    local win = vim.api.nvim_open_win(buf, true, {
        relative = "editor",
        width = width,
        height = height - 5,
        col = math.floor((ui.width - width) / 2),
        row = math.floor((ui.height - height) / 2),
        style = "minimal",
        border = "single",
    })
    vim.api.nvim_set_option_value("winhighlight", "Normal:Normal,FloatBorder:Normal", {
        win = win,
    })
    return buf, win
end

-- Fuzzy finder
local function fuzzy_file_finder()
    local fzf_cmd = "find . -type d \\( -name node_modules -o -name .git -o -name dist -o -name *cache* -o -name android -o -name ios -o -name .next \\) -prune -o -type f | fzf"
    local origin_win = vim.api.nvim_get_current_win()
    local term_buf, term_win = create_window()
    vim.cmd("startinsert")
    vim.fn.termopen({ "/bin/sh", "-c", fzf_cmd }, {
        on_exit = function(job_id, code, event)
            local raw_lines = vim.api.nvim_buf_get_lines(term_buf, 0, -1, false)
            vim.api.nvim_win_close(term_win, true)
            vim.api.nvim_buf_delete(term_buf, { force = true })
            local selected = nil
            for _, line in ipairs(raw_lines) do
                if line ~= "" then
                    selected = line
                    break
                end
            end
            if selected == nil then
                return
            end
            vim.api.nvim_set_current_win(origin_win)
            vim.cmd("edit " .. vim.fn.fnameescape(selected))
        end
    })
end
vim.keymap.set("n", "<leader>ff", fuzzy_file_finder, { noremap = true, silent = true })

-- Find files
local function find_files_to_qf(pattern)
    local cmd = "find . -type d \\( -name node_modules -o -name .git -o -name dist -o -name *cache* -o -name android -o -name ios -o -name .next \\) -prune -o -type f | grep -i " .. vim.fn.shellescape(pattern)
    local lines = vim.fn.systemlist(cmd)
    if #lines == 0 then
        return
    end
    local items = {}
    for _, line in ipairs(lines) do
        table.insert(items, { filename = line, lnum = 1, col = 1 })
    end
    vim.fn.setqflist({}, " ", { items = items, title = "Find Files: " .. pattern })
    vim.cmd("cw")
end
vim.api.nvim_create_user_command("FindFiles", function(opts)
    find_files_to_qf(opts.args)
end, { nargs = 1 })
vim.keymap.set("n", "<leader>fq", ":FindFiles<space>")

-- Git blame
local function git_blame_selection()
    local line_start = vim.fn.line("v")
    local line_end = vim.fn.line(".")
    local file = vim.fn.shellescape(vim.fn.expand("%"))
    local cmd = string.format(
        [[sh -c "echo '\033[1;31mBlame results:\033[0m' && git-blame-colored %s -L%d,%d"]], file, line_start, line_end
    )
    local term_buf = vim.api.nvim_create_buf(false, true)
    local term_win = vim.api.nvim_open_win(term_buf, true, {
        split = "below",
    })
    vim.fn.termopen({ "/bin/sh", "-c", cmd })
end
local function git_blame_file()
    local file = vim.fn.shellescape(vim.fn.expand("%"))
    local cmd = string.format([[sh -c "echo '\033[1;31mBlame results:\033[0m' && git-blame-colored %s"]], file)
    local term_buf = vim.api.nvim_create_buf(false, true)
    local term_win = vim.api.nvim_open_win(term_buf, true, {
        split = "below",
    })
    vim.fn.termopen({ "/bin/sh", "-c", cmd })
end
vim.keymap.set({'v', 'x'}, "gb", git_blame_selection, { noremap = true, silent = true })
vim.keymap.set('n', "gb", git_blame_file, { noremap = true, silent = true })

-- Open a scratch buffer
local function open_scratch_buffer()
    local bufnr = vim.api.nvim_create_buf(false, true)
    local scratch_win = vim.api.nvim_open_win(bufnr, true, {
        split = "above",
    })
    vim.api.nvim_win_set_buf(0, bufnr)
end
vim.keymap.set("n", "<leader>o", open_scratch_buffer, { noremap = true, silent = true })

-- Buffer to quickfix list
local function selection_to_qf()
    local startline = vim.fn.line("v")
    local endline = vim.fn.line(".")
    local lines = vim.fn.getline(startline, endline)
    if #lines == 0 then
        return
    end
    local items = {}
    for i, line in ipairs(lines) do
        local path, lnum, col, text = line:match("^(%S+):(%d+):(%d+):(.*)")
        if path == nil then
            path, lnum, text = line:match("^(%S+):(%d+):(.*)")
            col = 1
        end
        if path then
            table.insert(items, { filename = path, lnum = tonumber(lnum), col = tonumber(col), text = text })
        end
    end
    if #items ~= 0 then
        vim.fn.setqflist({}, " ", { items = items, title = "Selection to Quickfix" })
    end
    vim.cmd("cw")
end
vim.keymap.set({"v", "x"}, "<leader>q", selection_to_qf, { noremap = true, silent = true })

-- Statusline
vim.g.git_branch = ''
local function update_git_branch()
    local handle = io.popen("git rev-parse --abbrev-ref HEAD 2>/dev/null")
    if handle then
        local branch = handle:read("*l")
        handle:close()
        if branch then
            vim.g.git_branch = ' ' .. vim.trim(branch) .. ' | '
        else
            vim.g.git_branch = ''
        end
    else
        vim.g.git_branch = ''
    end
end
function git_branch()
    return vim.g.git_branch
end
augroup('GitBranchAutoUpdate', { clear = true })
autocmd({ 'BufEnter', 'FocusGained' }, {
    group = 'GitBranchAutoUpdate',
    pattern = { '*' },
    callback = update_git_branch,
})
function file_name()
    local name = vim.fn.expand('%')
    return name == '' and '[No Name]' or vim.fn.expand('%:.')
end
function get_diagnostics()
    local errors = vim.diagnostic.get(0, { severity = { vim.diagnostic.severity.ERROR } })
    local warnings = vim.diagnostic.get(0, { severity = { vim.diagnostic.severity.WARN } })
    local info = vim.diagnostic.get(0, { severity = { vim.diagnostic.severity.INFO } })
    local hints = vim.diagnostic.get(0, { severity = { vim.diagnostic.severity.HINT } })
    local diagnostic = {}
    if #errors > 0 then
        table.insert(diagnostic, 'E:' .. #errors)
    end
    if #warnings > 0 then
        table.insert(diagnostic, 'W:' .. #warnings)
    end
    if #info > 0 then
        table.insert(diagnostic, 'I:' .. #info)
    end
    if #hints > 0 then
        table.insert(diagnostic, 'H:' .. #hints)
    end
    return table.concat(diagnostic, ' ')
end
vim.o.statusline = '%{v:lua.git_branch()}%{v:lua.file_name()} %m %r%=%{v:lua.get_diagnostics()} %y | %l,%c'

-- Execute scripts
augroup('ExecuteScripts', { clear = true })
autocmd('FileType', {
    group = 'ExecuteScripts',
    pattern = { 'bash', 'sh' },
    callback = function()
        vim.keymap.set('n', '<leader>b', function()
            vim.cmd('%y')
            open_scratch_buffer()
            vim.cmd('put')
            vim.cmd('%!sh')
        end, { noremap = true, silent = true })
    end
})

-- Built-in session chooser
function session_chooser()
    local buf, win = create_window()
    local items = vim.fn.systemlist("ls -a ~/*.sock")
    vim.api.nvim_buf_set_lines(buf, 0, -1, false, items)
    vim.api.nvim_set_option_value("modifiable", false, { buf = buf })
    vim.api.nvim_set_option_value("readonly", true, { buf = buf })
    vim.keymap.set('n', '<CR>', function()
        local selected = vim.fn.getline('.')
        vim.api.nvim_win_close(win, true)
        if selected == nil then
            return
        end
        vim.cmd("connect " .. vim.fn.fnameescape(selected))
    end)
end
vim.keymap.set("n", "<leader>fs", session_chooser, { noremap = true, silent = true })

-- Plugins
local plugins = {
    'https://github.com/Exafunction/windsurf.vim',
    'https://github.com/neovim/nvim-lspconfig',
    'https://github.com/lewis6991/gitsigns.nvim',
    'https://github.com/brenoprata10/nvim-highlight-colors',
    'https://github.com/Ashik80/VimExplorer',
    'https://github.com/Ashik80/default16.vim',
}
local plugger = require("plugger")
plugger.setup(plugins)
vim.keymap.set('n', '<leader>pu', plugger.update, { noremap = true, silent = true })
vim.keymap.set('n', '<leader>pc', function() plugger.clean(plugins) end, { noremap = true, silent = true })

-- LSP
vim.lsp.enable({
    'basedpyright',
    'ts_ls',
    'eslint',
    'tailwindcss',
    'gopls',
    'terraformls',
    'jdtls',
})
vim.diagnostic.config({ virtual_text = true })

vim.keymap.set('n', '<leader>e', vim.diagnostic.open_float, { noremap = true, silent = true })
vim.keymap.set('n', 'grs', vim.lsp.buf.workspace_symbol, { noremap = true, silent = true })

vim.opt.completeopt = { "menuone", "noinsert", "popup" }
autocmd('LspAttach', {
    callback = function (ev)
        local bufnr = ev.buf
        local client = vim.lsp.get_client_by_id(ev.data.client_id)
        if client:supports_method("textDocument/completion") then
            vim.lsp.completion.enable(true, client.id, bufnr, {
                autotrigger = true,
                convert = function(item)
                    return { abbr = item.label:gsub("%b()", "") }
                end,
            })
        end
    end
})

-- Formatting
augroup('FormatPyWithBlack', { clear = true })
autocmd('BufWritePost', {
    group = 'FormatPyWithBlack',
    pattern = { '*.py' },
    callback = function()
        if vim.fn.executable('black') then
            vim.cmd('silent !black ' .. vim.fn.shellescape(vim.fn.expand('%')))
        end
    end
})
augroup('FormatJSWithPrettier', { clear = true })
autocmd('BufWritePost', {
    group = 'FormatJSWithPrettier',
    pattern = { '*.js', '*.jsx', '*.ts', '*.tsx' },
    callback = function()
        if vim.fn.executable('prettier') then
            vim.cmd('silent !prettier -w ' .. vim.fn.shellescape(vim.fn.expand('%')))
        end
    end
})

-- Git signs
vim.keymap.set('n', '<leader>gh', require('gitsigns').preview_hunk, { noremap = true, silent = true })

-- Colors
require('nvim-highlight-colors').setup({})

-- VimExplorer
vim.g.vimexplorer_show_hidden = 1
vim.g.vimexplorer_show_header = 0

-- Theme
vim.cmd [[ colorscheme default16 ]]
