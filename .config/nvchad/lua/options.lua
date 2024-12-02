require "nvchad.options"

-- add yours here!

local o = vim.o
-- o.cursorlineopt ='both' -- to enable cursorline!

vim.api.nvim_create_autocmd("FileType", {
  pattern = {"go", "gomod"},
  desc = "Go specific settings",
  callback = function ()
    o.shiftwidth = 4
    o.tabstop = 4
  end
})
