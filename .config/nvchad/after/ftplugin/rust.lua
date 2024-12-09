local bufnr = vim.api.nvim_get_current_buf()

local map = vim.keymap.set

map("n", "K", -- Override Neovim's built-in hover keymap with rustaceanvim's hover actions
  function()
    vim.cmd.RustLsp({ 'hover', 'actions' })
  end,
  { silent = true, buffer = bufnr, desc = "LSP hover documentaion" }
)
map("n", "<leader>rn", vim.lsp.buf.rename, { silent = true, buffer = bufnr, desc = "LSP rename symbol" })
map("n", "<leader>ca", vim.lsp.buf.code_action, { silent = true, buffer = bufnr, desc = "LSP code actions" })
