return {
  {
    "stevearc/conform.nvim",
    event = 'BufWritePre', -- uncomment for format on save
    opts = require "configs.conform",
  },

  {
    "neovim/nvim-lspconfig",
    config = function()
      require "configs.lspconfig"
    end,
  },

  {
    "hrsh7th/nvim-cmp",
    opts = function(_, opts)
      local cmp = require("cmp")
      opts.mapping = cmp.mapping.preset.insert({
        ["<C-y>"] = cmp.mapping.confirm({ select = true })
      })
    end
  },

  {
    "nvim-treesitter/nvim-treesitter",
    opts = require "configs.treesitter"
  },

  {
    "windwp/nvim-ts-autotag",
    lazy = false,
    opts = {}
  },

  {
    'mrcjkb/rustaceanvim',
    version = '^5',
    lazy = false,
  },

  -- disable indentation guides
  { "lukas-reineke/indent-blankline.nvim", enabled = false }
}
