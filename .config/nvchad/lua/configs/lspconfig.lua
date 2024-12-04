-- load defaults i.e lua_lsp
require("nvchad.configs.lspconfig").defaults()

local lspconfig = require "lspconfig"

local servers = { "html", "cssls", "gopls", "vtsls", "eslint", "pyright" }
local nvlsp = require "nvchad.configs.lspconfig"

local custom_mappings = function(client, bufnr)
  nvlsp.on_attach(client, bufnr)

  local map = vim.keymap.set

  map("n", "gd", "<Nop>", { buffer = bufnr })
  map("n", "gd", "yiwgg/<C-r>0<CR>:noh<CR>", { noremap = true, buffer = bufnr })
  map("n", "<leader>fr", vim.lsp.buf.references, { desc = "LSP find references", buffer = bufnr })
  map("n", "<leader>rn", vim.lsp.buf.rename, { desc = "LSP rename symbol", buffer = bufnr })
end

-- lsps with default config
for _, lsp in ipairs(servers) do
  lspconfig[lsp].setup {
    on_attach = custom_mappings,
    on_init = nvlsp.on_init,
    capabilities = nvlsp.capabilities,
  }
end

-- configuring single server, example: typescript
-- lspconfig.ts_ls.setup {
--   on_attach = nvlsp.on_attach,
--   on_init = nvlsp.on_init,
--   capabilities = nvlsp.capabilities,
-- }
