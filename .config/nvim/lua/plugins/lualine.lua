return {
  'nvim-lualine/lualine.nvim',
  lazy = false,
  dependencies = { 'nvim-tree/nvim-web-devicons' },
  opts = {
    options = {
      globalstatus = true,
      disabled_filetypes = { 'NvimTree' },
      ignore_focus = { 'NvimTree' },
      -- component_separators = { left = '╱', right = '╲' },
      -- section_separators = { left = '◤', right = '◥' },
    }
  }
}
