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
vim.o.wildignore = "**/.git/*,**/node_modules/**,**/dist/**,**/tmp/**,**/ios/**,**/android/**"
vim.o.path = "**"
vim.o.hlsearch = false
vim.o.splitbelow = true
vim.o.splitright = true
vim.o.signcolumn = "yes"
vim.o.clipboard = "unnamedplus"
vim.o.writebackup = false
vim.o.winborder = "solid"
vim.o.guicursor = ""
-- vim.o.listchars = "tab:▸ ,trail:·"
vim.o.listchars = "tab:  ,trail:·"
vim.o.list = true
vim.o.fillchars = "eob: "
vim.o.wildmode = "noselect:lastused,full"
vim.o.wildoptions = "pum,fuzzy"
vim.o.pumheight = 15

local augroup = vim.api.nvim_create_augroup
local autocmd = vim.api.nvim_create_autocmd

-- Mappings
vim.keymap.set('n', '-', '<cmd>Ex<CR>')
vim.keymap.set({'v', 'x'}, '>', '>gv')
vim.keymap.set({'v', 'x'}, '<', '<gv')
vim.keymap.set({'v', 'x'}, 'J', ":move '>+1<CR>gv=gv")
vim.keymap.set({'v', 'x'}, 'K', ":move '<-2<CR>gv=gv")
vim.keymap.set('n', '<leader>cp', ':let @+ = expand("%:.")<CR>')
vim.keymap.set('t', '<C-w>N', '<C-\\><C-n>')
vim.keymap.set('t', '<C-w>w', '<C-\\><C-n><C-w>w')
vim.keymap.set('t', '<C-w><C-w>', '<C-\\><C-n><C-w>w')
vim.keymap.set("n", "<leader>ff", ":find<space>")

-- Lazygit
if vim.fn.executable("lazygit") == 1 then
    vim.keymap.set('n', '<leader>lg', ':tabnew | term lazygit<CR>i')
end

augroup('LazyGitAutoClose', { clear = true })
autocmd('TermClose', {
    group = 'LazyGitAutoClose',
    pattern = 'term://*lazygit',
    callback = function()
        vim.api.nvim_input('<CR>')
    end
})

-- Grepping
vim.o.grepprg = "rg --no-heading --column"
vim.keymap.set('n', '<leader>fg', ':grep!<space>')

-- Finding
files_cache = {}
function FindFunc(cmdarg, cmdline)
  if #files_cache == 0 then
    local cmd = string.format(
      [[find . -type d \( -name node_modules -o -name .git -o -name dist -o -name *_cache -o -name __pycache__ -o -name android -o -name ios \) -prune -o -type f -print]]
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

-- Fuzzy finder
local function fuzzy_file_finder()
    local cmd = "cat {}"
    if vim.fn.executable("batcat") == 1 then
        cmd = "batcat --theme=ansi --style=numbers --color=always {}"
    end
    local preview_cmd = vim.fn.shellescape(cmd)
    local fzf_cmd = string.format("rg --files | fzf --preview=%s", preview_cmd)
    local term_buf = vim.api.nvim_create_buf(false, true)
    vim.api.nvim_win_set_buf(0, term_buf)
    vim.cmd("startinsert")

    vim.fn.termopen({ "/bin/sh", "-c", fzf_cmd }, {
        on_exit = function(job_id, code, event)
            local raw_lines = vim.api.nvim_buf_get_lines(term_buf, 0, -1, false)
            if #raw_lines == 0 then
                vim.api.nvim_buf_delete(term_buf, { force = true })
                return
            end
            local lines = {}
            for _, line in ipairs(raw_lines) do
                if line == "" then
                    break
                end
                table.insert(lines, { filename = line, lnum = 1, col = 1 })
            end
            if #lines == 0 then
                vim.api.nvim_input('<CR>')
            end
            vim.api.nvim_buf_delete(term_buf, { force = true })
            vim.fn.setqflist({}, " ", { items = lines, title = "Fuzzy Find: " })
            vim.cmd("silent cfirst")
        end
    })
end
vim.keymap.set("n", "<leader>fz", fuzzy_file_finder, { noremap = true, silent = true })

-- Find files
local function find_files_to_qf(pattern)
    local cmd = "rg --files | rg -i " .. vim.fn.shellescape(pattern)
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

-- Netrw copy file
augroup('NetrwCopyFilePath', { clear = true })
autocmd('FileType', {
    group = 'NetrwCopyFilePath',
    pattern = 'netrw',
    callback = function()
        local function get_file()
            return vim.fn['netrw#Call']('NetrwFile', vim.fn['netrw#Call']('NetrwGetWord'))
        end
        local function copy_file()
            local file = vim.fn.fnamemodify(get_file(), ":.")
            vim.fn.setreg('+', file)
            print("Copied file path to clipboard: ", file)
        end
        local function xdg_open_file()
            local file = vim.fn.fnamemodify(get_file(), ":.")
            vim.fn.system({ "xdg-open", file })
        end
        vim.keymap.set('n', 'cp', copy_file, { noremap = true, silent = true })
        vim.keymap.set('n', 'go', xdg_open_file, { noremap = true, silent = true })
    end
})

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
vim.api.nvim_create_augroup('GitBranchAutoUpdate', { clear = true })
vim.api.nvim_create_autocmd({ 'BufEnter', 'FocusGained' }, {
  group = 'GitBranchAutoUpdate',
  pattern = { '*' },
  callback = update_git_branch,
})
function file_name()
  local name = vim.fn.expand('%')
  return name == '' and '[No Name]' or vim.fn.expand('%:.')
end
vim.o.statusline = '%{v:lua.git_branch()}%{v:lua.file_name()} %m %r%=%y | %l,%c'

-- Plugins
vim.pack.add({
    -- { src = 'https://github.com/nanotech/jellybeans.vim' },
    { src = 'https://github.com/vague2k/vague.nvim' },
    { src = 'https://github.com/Exafunction/windsurf.vim', version = 'main' },
    { src = 'https://github.com/neovim/nvim-lspconfig' },
    { src = 'https://github.com/lewis6991/gitsigns.nvim' },
    { src = 'https://github.com/brenoprata10/nvim-highlight-colors' },
    { src = 'https://github.com/nvim-treesitter/nvim-treesitter' }
})

-- Treesitter
require'nvim-treesitter.configs'.setup({
    highlight = {
        enable = true,
        additional_vim_regex_highlighting = false,
    },
    indent = {
        enable = true
    }
})

-- LSP
vim.lsp.enable({
    'basedpyright',
    'ts_ls',
    'eslint',
    'tailwindcss',
    'gopls',
    'terraformls',
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

-- Theme
vim.cmd [[
    "colorscheme jellybeans
    "hi Normal ctermbg=NONE guibg=NONE
    "hi SignColumn ctermbg=NONE guibg=NONE
    "hi StatusLine gui=none ctermfg=188 guifg=#e8e8d3 guibg=NONE ctermbg=NONE
    colorscheme vague
    hi StatusLine guibg=None ctermbg=None
]]
