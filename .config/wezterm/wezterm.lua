local wezterm = require 'wezterm'
local config = {}

-- Font settings
config.font = wezterm.font('FiraCode Nerd Font', { weight = 'Medium' })
config.font_size = 15
config.line_height = 1.3

-- General
config.audible_bell = 'Disabled'
config.hide_tab_bar_if_only_one_tab = true
config.window_padding = {
    bottom = 0
}

-- Colors
config.color_scheme = 'Ayu Dark (Gogh)'

return config
