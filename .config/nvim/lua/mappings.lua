local map = vim.keymap.set

map('x', '>', '>gv', { desc = 'Indent right' })
map('x', '<', '<gv', { desc = 'Indent left' })
map('n', '<Esc>', '<cmd>noh<CR>', { desc = 'Remove highlight' })

map('n', '<C-n>', '<cmd>NvimTreeToggle<CR>', { desc = 'Toggle file tree' })

map('n', '<C-h>', '<C-W>h')
map('n', '<C-j>', '<C-W>j')
map('n', '<C-k>', '<C-W>k')
map('n', '<C-l>', '<C-W>l')
