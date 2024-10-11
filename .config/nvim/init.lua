vim.g.mapleader = " "
vim.o.number = true
vim.o.expandtab = true
vim.o.shiftwidth = 4
vim.o.tabstop = 4
vim.o.ignorecase = true
vim.o.smartcase = true
vim.o.hlsearch = false
vim.o.swapfile = false
vim.o.signcolumn = 'yes'
vim.o.guicursor = ''

vim.cmd.colorscheme "base16-classic-dark"

vim.api.nvim_create_autocmd({"BufEnter", "BufWinEnter"}, {
  pattern = {"*.js", "*.jsx", "*.ts", "*.tsx", "*.json", "*.rb", "*.yml", "*.lua"},
  callback = function()
    vim.o.shiftwidth = 2
    vim.o.tabstop = 2
  end
})

-- Handmade compiler
function Compile(cmd)
  vim.fn.jobstart(cmd, {
    stdout_buffered = true,
    on_stdout = function(_, data, _)
      local lines = {}
      if data then
        for _, line in ipairs(data) do
          if line and line ~= "" then
            table.insert(lines, line)
          end
        end
      end
      vim.fn.setqflist({}, 'r', { lines = lines })
      vim.cmd("cw")
    end
  })
end

-- File fuzzy finder
function FuzzyFindFile()
  vim.cmd("enew")
  vim.fn.termopen('fzf | sed "s/$/:0:0/" > /tmp/filefind', {
    on_exit = function()
      vim.o.efm = '%f:%l:%c'
      vim.cmd("bd!")
      vim.cmd('silent cfile /tmp/filefind')
    end
  })
  vim.cmd('startinsert')
end
vim.keymap.set('n', '<leader>ff', ':lua FuzzyFindFile()<CR>')

-- Taken inspiration from Erik's blog (https://blog.erikwastaken.dev/posts/2023-05-06-a-case-for-neovim-without-plugins.html)
local function RunCustomServer(namespace, command, pattern, groups, severity_map)
  vim.fn.jobstart(command, {
    stdout_buffered = true,
    on_stdout = function(_, data)
      local ns = vim.api.nvim_create_namespace(namespace)
      if data then
        local ds = {}
        for _, line in ipairs(data) do
          local d = vim.diagnostic.match(line, pattern, groups, severity_map)
          if d then
            table.insert(ds, d)
          end
        end
        vim.diagnostic.set(ns, 0, ds, {})
        vim.diagnostic.show()
      end
    end,
  })
end

vim.keymap.set("n", "<leader>e", ":lua vim.diagnostic.open_float()<CR>")
vim.api.nvim_create_autocmd("LspAttach", {
  callback = function (args)
    vim.keymap.set("n", "<leader>fr", ":lua vim.lsp.buf.references()<CR>")
    vim.keymap.set("n", "<leader>gd", ":lua vim.lsp.buf.definition()<CR>")
    vim.keymap.set("n", "<leader>ca", ":lua vim.lsp.buf.code_action()<CR>")
    vim.keymap.set("n", "<leader>rn", ":lua vim.lsp.buf.rename()<CR>")
  end
})

-- Eslint settings
local function RunESLint()
  local filename = vim.fn.expand("%")
  RunCustomServer(
    "ESlint",
    { "bash", "-c", "eslint_d " .. filename .. " -f compact | head -n -2" },
    ": line (%d+), col (%d+), (%w+) %- (.+)",
    { "lnum", "col", "severity", "message" },
    {
      Error = vim.diagnostic.severity.ERROR,
      Warning = vim.diagnostic.severity.WARN
    }
  )
end

vim.api.nvim_create_autocmd({"BufEnter", "BufWritePost"}, {
  pattern = {"*.js", "*.jsx", "*.ts", "*.tsx"},
  callback = function(args)
    local eslint_root = vim.fs.root(args.buf, {".eslintrc", ".eslintrc.js", ".eslintrc.json"})
    if eslint_root then
      RunESLint()
    end
  end
})

-- JavaScript/TypeScript settings
vim.api.nvim_create_autocmd("FileType", {
  pattern = {"javascript", "javascriptreact", "typescript", "typescriptreact"},
  callback = function(args)
    vim.o.efm = "%f(%l\\,%c): %m"
    vim.lsp.start({
      name = "tsserver",
      cmd = {"typescript-language-server", "--stdio"},
      root_dir = vim.fs.root(args.buf, {"package.json"})
    })
  end
})

local function FormatWithPrettier()
  local filename = vim.fn.expand("%")
  vim.cmd("silent !prettier -w " .. filename)
end

vim.api.nvim_create_autocmd("BufWritePost", {
  pattern = {"*.js", "*.jsx", "*.ts", "*.tsx"},
  callback = function()
    FormatWithPrettier()
  end
})

-- Python settings
local function FormatWithBlack()
  local filename = vim.fn.expand("%")
  vim.cmd("silent !black " .. filename)
end

vim.api.nvim_create_autocmd({"BufWritePost"}, {
  pattern = "*.py",
  callback = function(args)
    FormatWithBlack()
  end
})

vim.api.nvim_create_autocmd("FileType", {
  pattern = "python",
  callback = function(args)
    vim.o.efm = "%f:%l:%c %m"
    vim.lsp.start({
      name = "pyright-langserver",
      cmd = {"pyright-langserver", "--stdio"},
      settings = {
        python = {
          analysis = {
            autoSearchPaths = true,
            diagnosticMode = "openFilesOnly",
            useLibraryCodeForTypes = true
          }
        }
      },
      root_dir = vim.fs.root(args.buf, {"requirements.txt"}), 
      single_file_support = true
    })
  end
})

-- Go settings
vim.api.nvim_create_autocmd("FileType", {
  pattern = {"go", "gomod"},
  callback = function(args)
    vim.o.efm = '%f:%l:%c: %m'
    vim.lsp.start({
      name = "gopls",
      cmd = {"gopls"},
      settings = {
        gopls = {
          semanticTokens = true,
        },
      },
      root_dir = vim.fs.root(args.buf, {"go.mod"}),
      single_file_support = true
    })
  end
})

local function FormatGo()
  local filename = vim.fn.expand("%")
  vim.cmd("silent !gofmt -w " .. filename)
end

vim.api.nvim_create_autocmd("BufWritePost", {
  pattern = {"*.go"},
  callback = function()
    FormatGo()
  end
})

-- Disable all lsp highlightings
-- for _, group in ipairs(vim.fn.getcompletion("@lsp", "highlight")) do
--   vim.api.nvim_set_hl(0, group, {})
-- end
