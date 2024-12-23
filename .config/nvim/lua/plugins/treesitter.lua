return {
  'nvim-treesitter/nvim-treesitter',
  build = ':TSUpdate',
  config = function()
    ---@diagnostic disable-next-line: missing-fields
    require 'nvim-treesitter.configs'.setup {
      ensure_installed = {
        "lua",
        "vimdoc",
        "python",
        "typescript",
        "javascript",
        "tsx",
        "html",
        "css",
        "json",
        "yaml",
        "go",
        "rust"
      },
      sync_install = false,
      ignore_install = {},
      auto_install = false,
      highlight = {
        enable = true,
        additional_vim_regex_highlighting = false,
      },
    }
  end
}
