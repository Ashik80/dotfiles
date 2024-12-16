return {
  {
    'notken12/base46-colors',
    lazy = false,
    priority = 1000,
    config = function()
      vim.cmd.colorscheme 'ayu_dark'
      -- nvim tree colors
      vim.cmd [[
        hi NvimTreeWinSeparator guifg=bg
      ]]
    end
  }
}
