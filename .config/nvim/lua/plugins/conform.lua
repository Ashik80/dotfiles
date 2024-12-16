return {
  'stevearc/conform.nvim',
  event = 'BufWritePre',
  opts = {
    formatters_by_ft = {
      typescript = { 'prettier' },
      typescriptreact = { 'prettier' },
      javascript = { 'prettier' },
      javascriptreact = { 'prettier' },
      go = { 'gofmt' },
      html = { 'prettier' },
      css = { 'prettier' },
      python = { 'black' },
    },
    format_on_save = {
      lsp_format = 'fallback'
    }
  },
}
