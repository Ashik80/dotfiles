require "nvchad.mappings"

local map = vim.keymap.set
local nomap = vim.keymap.del

-- telescope
local telescope_builtin = require("telescope.builtin")
nomap("n", "<leader>fw")
map("n", "<leader>fg", telescope_builtin.live_grep, { desc = "telescope live grep", noremap = true })

-- diagnostics
nomap("n", "<leader>e")
nomap("n", "<C-w>d")
nomap("n", "<C-w><C-d>")
map("n", "<leader>e", vim.diagnostic.open_float, { desc = "show diagnostics under the cursor" })

-- lsp
nomap("n", "<leader>rn")
