vim.g.mapleader = " "

-- Disable netrw
vim.g.loaded_netrw = 1
vim.g.loaded_netrwPlugin = 1

local opt = vim.opt

opt.number = true
opt.tabstop = 4
opt.shiftwidth = 4
opt.expandtab = true
opt.signcolumn = 'yes'
opt.termguicolors = true
opt.showmode = false
opt.fillchars = 'eob: '
opt.clipboard = 'unnamedplus'
opt.ignorecase = true
opt.smartcase = true
opt.laststatus = 3
