return {
  'nvim-tree/nvim-tree.lua',
  cmd = { 'NvimTreeToggle', 'NvimTreeFocus' },
  dependencies = { 'nvim-tree/nvim-web-devicons' },
  config = function()
    require("nvim-tree").setup {
      renderer = {
        root_folder_label = false
      },
      update_focused_file = {
        enable = true,
      }
    }
  end
}
