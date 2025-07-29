return {
  'stevearc/oil.nvim',
  cmd = { 'Oil' },
  dependencies = { "nvim-tree/nvim-web-devicons" },
  config = function()
    require('oil').setup()
  end
}
