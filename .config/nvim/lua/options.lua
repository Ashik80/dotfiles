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
opt.guicursor = ''
opt.splitright = true
opt.splitbelow = true

-- vim.cmd.colorscheme "retrobox"

vim.cmd [[
augroup Highlights
    autocmd!
    " autocmd ColorScheme * hi DiagnosticError ctermfg=1 guifg=Red
    " autocmd ColorScheme * hi DiagnosticWarn ctermfg=3 guifg=Orange
    " autocmd ColorScheme * hi DiagnosticInfo ctermfg=4 guifg=LightBlue
    " autocmd ColorScheme * hi DiagnosticHint ctermfg=7 guifg=LightGrey
    " autocmd ColorScheme * hi DiagnosticUnderlineError cterm=underline gui=underline guisp=Red
    " autocmd ColorScheme * hi DiagnosticUnderlineWarn cterm=underline gui=underline guisp=Orange
    " autocmd ColorScheme * hi DiagnosticUnderlineInfo cterm=underline gui=underline guisp=LightBlue
    " autocmd ColorScheme * hi DiagnosticUnderlineHint cterm=underline gui=underline guisp=LightGrey
    autocmd ColorScheme * hi DiagnosticUnderlineError cterm=underline gui=underline
    autocmd ColorScheme * hi DiagnosticUnderlineWarn cterm=underline gui=underline
    autocmd ColorScheme * hi DiagnosticUnderlineInfo cterm=underline gui=underline
    autocmd ColorScheme * hi DiagnosticUnderlineHint cterm=underline gui=underline

    autocmd ColorScheme * hi link VirtualTextError DiagnosticError
    autocmd ColorScheme * hi link VirtualTextWarn DiagnosticWarn
    autocmd ColorScheme * hi link VirtaulTextInfo DiagnosticWarn
    autocmd ColorScheme * hi link VirtaulTextHint DiagnosticHint

    autocmd ColorScheme * hi link DiagnosticSignError DiagnosticError
    autocmd ColorScheme * hi link DiagnosticSignWarn DiagnosticWarn
    autocmd ColorScheme * hi link DiagnosticSignInfo DiagnosticWarn
    autocmd ColorScheme * hi link DiagnosticSignHint DiagnosticHint
augroup END
]]
