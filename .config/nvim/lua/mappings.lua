local map = vim.keymap.set

map('x', '>', '>gv', { desc = 'Indent right' })
map('x', '<', '<gv', { desc = 'Indent left' })
map('x', 'J', ":move '>+1<CR>gv", { desc = 'Move line down' })
map('x', 'K', ":move '>-2<CR>gv", { desc = 'Move line up' })
map('n', '<Esc>', '<cmd>noh<CR>', { desc = 'Remove highlight' })

map('n', '<C-n>', '<cmd>NvimTreeToggle<CR>', { desc = 'Toggle file tree' })
map('n', 'ge', '<cmd>Oil<CR>', { desc = 'Open oil' })

map('n', '<C-h>', '<C-W>h')
map('n', '<C-j>', '<C-W>j')
map('n', '<C-k>', '<C-W>k')
map('n', '<C-l>', '<C-W>l')

map('t', '<Esc><Esc>', '<C-\\><C-n>', { desc = 'Go to normal mode on terminal' })
