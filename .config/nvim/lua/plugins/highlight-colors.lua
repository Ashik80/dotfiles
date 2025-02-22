return {
  'brenoprata10/nvim-highlight-colors',
  lazy = false,
  event = { 'BufReadPre', 'BufNewFile' },
  config = function()
    require('nvim-highlight-colors').setup {
      render = 'virtual',
      enable_tailwind = true,
      virtual_symbol_prefix = ' ',
      virtual_symbol_suffix = ' ',
    }
  end
}
