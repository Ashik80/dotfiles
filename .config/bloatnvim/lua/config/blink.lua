return {
  completion = {
    accept = {
      auto_brackets = {
        enabled = true
      }
    },
    menu = {
      -- border = 'rounded',
      -- winhighlight = "Normal:BlinkCmpDoc,FloatBorder:BlinkCmpDocBorder,CursorLine:BlinkCmpDocCursorLine,Search:None",
      draw = {
        padding = { 1, 1 },
        treesitter = { 'lsp' },
        columns = { { "label", "label_description", gap = 1 }, { "kind_icon", gap = 1 }, { "kind" } },
      }
    },
    documentation = {
      auto_show = true,
      auto_show_delay_ms = 200,
    },
  },
  keymap = { preset = 'default' },
  sources = {
    default = { 'lsp', 'path', 'snippets', 'buffer' },
  },
  cmdline = {
    enabled = false
  },
  signature = { enabled = true }
}
