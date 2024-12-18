return {
  'stevearc/oil.nvim',
  dependencies = { "nvim-tree/nvim-web-devicons" },
  config = function()
    require('oil').setup()
    vim.keymap.set('n', 'ge', '<cmd>Oil<CR>', { desc = 'Open oil' })
  end
}
