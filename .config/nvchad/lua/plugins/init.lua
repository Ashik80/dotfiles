return {
  {
    "stevearc/conform.nvim",
    event = 'BufWritePre', -- uncomment for format on save
    opts = require "configs.conform",
  },

  -- These are some examples, uncomment them if you want to see them work!
  {
    "neovim/nvim-lspconfig",
    config = function()
      require "configs.lspconfig"
    end,
  },

  {
    "hrsh7th/nvim-cmp",
    opts = function (_, opts)
      local cmp = require("cmp")
      opts.mapping = cmp.mapping.preset.insert({
        ["<C-y>"] = cmp.mapping.confirm({ select = true })
      })
    end
  },

  {
  	"nvim-treesitter/nvim-treesitter",
  	opts = {
  		ensure_installed = {
        "html",
        "go",
        "typescript",
        "tsx",
        "python",
  		},
  	},
  },

  {
    "windwp/nvim-ts-autotag",
    lazy = false,
    opts = {}
  },

  -- disable indentation guides
  { "lukas-reineke/indent-blankline.nvim", enabled = false }
}
