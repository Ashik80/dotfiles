return {
  {
    'williamboman/mason.nvim',
    cmd = { "Mason", "MasonInstall", "MasonInstallAll", "MasonUpdate" },
    dependencies = { 'williamboman/mason-lspconfig.nvim' },
    config = function()
      require("mason").setup()
      require("mason-lspconfig").setup({
        ensure_installed = {
          'lua_ls',
          "pyright",
          "vtsls",
          "eslint",
          "gopls"
        }
      })
    end
  },
  {
    'neovim/nvim-lspconfig',
    event = { "BufReadPre", "BufNewFile" },
    dependencies = { 'williamboman/mason.nvim', 'saghen/blink.cmp' },
    config = function()
      require('config.lspconfig')
    end
  },
  {
    'saghen/blink.cmp',
    dependencies = 'rafamadriz/friendly-snippets',
    event = 'InsertEnter',
    version = 'v0.*',
    opts = require('config.blink'),
    opts_extend = { "sources.default" }
  },
  -- {
  --   'hrsh7th/cmp-nvim-lsp',
  --   event = 'InsertEnter',
  --   enabled = false,
  --   dependencies = {
  --     'hrsh7th/cmp-buffer',
  --     'hrsh7th/cmp-path',
  --     'hrsh7th/nvim-cmp',
  --     'L3MON4D3/LuaSnip',
  --     'saadparwaiz1/cmp_luasnip',
  --     'windwp/nvim-autopairs',
  --   },
  --   config = function()
  --     require('config.cmp')
  --   end
  -- }
}
