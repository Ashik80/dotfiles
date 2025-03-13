return {
  {
    "nvchad/ui",
    enabled = false,
    config = function()
      require "nvchad"
    end
  },

  {
    "nvchad/base46",
    lazy = true,
    enabled = false,
    build = function()
      require("base46").load_all_highlights()
    end,
  },

  {
    'sainnhe/everforest',
    lazy = false,
    priority = 1000,
    enabled = false,
    config = function()
      vim.g.everforest_enable_italic = true
      vim.g.everforest_background = 'hard'
      vim.g.everforest_better_performance = 1
      vim.cmd.colorscheme('everforest')
    end
  }
}
