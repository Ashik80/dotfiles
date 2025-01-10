return {
  {
    'notken12/base46-colors',
    lazy = false,
    enabled = false,
    priority = 1000,
    config = function()
      vim.cmd.colorscheme 'ayu_dark'
      -- nvim tree colors
      vim.cmd [[
        hi NvimTreeWinSeparator guifg=bg
      ]]
    end
  },

  {
    "nvchad/ui",
    config = function()
      require "nvchad"
    end
  },

  {
    "nvchad/base46",
    lazy = true,
    build = function()
      require("base46").load_all_highlights()
    end,
  },
}
