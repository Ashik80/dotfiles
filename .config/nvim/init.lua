vim.o.number = true
vim.o.expandtab = true
vim.o.shiftwidth = 4
vim.o.tabstop = 4
vim.o.ignorecase = true
vim.o.smartcase = true
vim.o.hlsearch = false
vim.o.swapfile = false
vim.o.signcolumn = 'yes'

vim.cmd.colorscheme "habamax"

vim.api.nvim_create_autocmd({"BufEnter", "BufWinEnter"}, {
  pattern = {"*.js", "*.jsx", "*.ts", "*.tsx", "*.json", "*.lua"},
  callback = function()
    vim.o.shiftwidth = 2
    vim.o.tabstop = 2
  end
})

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

vim.api.nvim_create_autocmd("FileType", {
  pattern = {"javascript", "javascriptreact", "typescript", "typescriptreact"},
  callback = function(args)
    vim.lsp.start({
      name = "tsserver",
      cmd = {"typescript-language-server", "--stdio"},
      root_dir = vim.fs.root(args.buf, {"package.json"})
    })
  end
})

local function RunPyright()
  local filename = vim.fn.expand("%")
  RunCustomServer(
    "Pyright",
    { "bash", "-c", "pyright " .. filename .. " | head -n -1 | tail -n +2 | sed \'s/^\\s*//\'" },
    ":(%d+):(%d+) %- (%w+): (.+)",
    { "lnum", "col", "severity", "message" },
    {
      warning = vim.diagnostic.severity.WARN,
      error = vim.diagnostic.severity.ERROR
    }
  )
end

local function FormatWithBlack()
  local filename = vim.fn.expand("%")
  vim.cmd("silent !black " .. filename)
end

vim.api.nvim_create_autocmd({"BufEnter", "BufWritePost"}, {
  pattern = "*.py",
  callback = function(args)
    RunPyright()
    FormatWithBlack()
  end
})

vim.api.nvim_create_autocmd("FileType", {
  pattern = "python",
  callback = function(args)
    vim.lsp.start({
      name = "pyright",
      cmd = {"pyright-langserver", "--stdio"},
      single_file_support = true,
    })
    -- Disable LSP diagnostics
    vim.lsp.handlers["textDocument/publishDiagnostics"] = function() end
  end
})
