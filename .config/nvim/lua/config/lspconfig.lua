local orig_util_open_floating_preview = vim.lsp.util.open_floating_preview
---@diagnostic disable-next-line: duplicate-set-field
function vim.lsp.util.open_floating_preview(contents, syntax, opts, ...)
  opts = opts or {}
  ---@diagnostic disable-next-line: inject-field
  opts.border = 'rounded'
  return orig_util_open_floating_preview(contents, syntax, opts, ...)
end

local lspconfig = require('lspconfig')
local capabilities = vim.lsp.protocol.make_client_capabilities()
capabilities = require('cmp_nvim_lsp').default_capabilities()

local servers = { "lua_ls", "pyright", "vtsls", "gopls", "eslint" }

local map = vim.keymap.set

local function on_attach()
  map('n', 'grr', vim.lsp.buf.references, { desc = 'LSP show references' })
  map('n', 'grn', vim.lsp.buf.rename, { desc = 'LSP show references' })
  map('n', 'gca', vim.lsp.buf.code_action, { desc = 'LSP show references' })
end

map('n', '<leader>e', vim.diagnostic.open_float, { desc = 'Show diagnostic in hover' })
map('n', '<leader>de', vim.diagnostic.setloclist, { desc = 'Send buffer diagnostics to location list' })
map('n', '<leader>da', vim.diagnostic.setqflist, { desc = 'Send all diagnostics to quickfix list' })

for _, server in ipairs(servers) do
  lspconfig[server].setup {
    capabilities = capabilities,
    on_attach = on_attach,
  }
end
