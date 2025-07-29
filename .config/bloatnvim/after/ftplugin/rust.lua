local map = vim.keymap.set

map('n', 'grr', vim.lsp.buf.references, { desc = 'LSP show references' })
map('n', 'grn', vim.lsp.buf.rename, { desc = 'LSP show references' })
map('n', 'gca', vim.lsp.buf.code_action, { desc = 'LSP show references' })
