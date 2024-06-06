vim.g.mapleader = ' '
vim.g.maplocalleader = ' '

vim.opt.number = true
vim.opt.mouse = 'a'
vim.opt.showmode = false

vim.opt.guicursor = ""

vim.opt.wrap = false

vim.opt.tabstop = 4
vim.opt.shiftwidth = 4

vim.opt.ignorecase = true
vim.opt.smartcase = true
vim.opt.wildignorecase = true

vim.opt.signcolumn = 'yes'

-- Configure how new splits should be opened
vim.opt.splitright = true
vim.opt.splitbelow = true

-- vim.opt.list = true
-- vim.opt.listchars = { tab = '» ', trail = '·', nbsp = '␣' }

-- Preview substitutions live, as you type!
vim.opt.inccommand = 'split'

-- Show which line your cursor is on
vim.opt.cursorline = true

-- Minimal number of screen lines to keep above and below the cursor.
vim.opt.scrolloff = 10

-- Fold configuration
vim.opt.foldmethod = 'indent'
vim.opt.foldlevel = 99

-- Set highlight on search, but clear on pressing <Esc> in normal mode
vim.opt.hlsearch = false

-- Diagnostic keymaps
vim.keymap.set('n', '[d', vim.diagnostic.goto_prev, { desc = 'Go to previous [D]iagnostic message' })
vim.keymap.set('n', ']d', vim.diagnostic.goto_next, { desc = 'Go to next [D]iagnostic message' })
vim.keymap.set('n', '<leader>e', vim.diagnostic.open_float, { desc = 'Show diagnostic [E]rror messages' })
vim.keymap.set('n', '<leader>q', vim.diagnostic.setloclist, { desc = 'Open diagnostic [Q]uickfix list' })

-- Terminal mappings
vim.keymap.set('t', '<C-w>N', '<C-\\><C-n>', { desc = 'Exit terminal mode' })

-- Quit everything without saving
vim.keymap.set('n', '<leader>q', '<cmd>qa!<CR>', { desc = '[Q]uit everything' })

-- Copy current file name
vim.keymap.set('n', '<leader>yf', '<cmd>let @f =  expand("%:.")<CR>', { desc = '[Y]ank current [F]ile name' })

-- Paste from clipboard
vim.keymap.set({'n', 'v'}, '<leader>y', '"+y', { desc = 'Yank to clipboard' })
vim.keymap.set({'n', 'v'}, '<leader>cp', '"+p', { desc = 'Paste form clipboard' })

-- Indent manually
vim.keymap.set('v', '>', '>gv', { desc = 'Indent to the right' })
vim.keymap.set('v', '<', '<gv', { desc = 'Indent to the left' })

-- Navigate quickfix list
vim.keymap.set('n', '<C-n>', '<cmd>cn<CR>', { desc = 'Go to next item in the quickfix list' })
vim.keymap.set('n', '<C-p>', '<cmd>cp<CR>', { desc = 'Go to previous item in the quickfix list' })

-- Substitute all occurences of word in line
vim.keymap.set('v', '<leader>s', 'y:s/<c-r>"//g<left><left>', { desc = '[S]ubstitute visually selected word' })

-- surround word with quotes
vim.keymap.set('v', '<leader>"', 'c"<c-r>""<esc>')
vim.keymap.set('v', '<leader>\'', 'c\'<c-r>"\'<esc>')
vim.keymap.set('v', '<leader>)', 'c(<c-r>")<esc>')
vim.keymap.set('v', '<leader>}', 'c{<c-r>"}<esc>')
vim.keymap.set('v', '<leader>]', 'c[<c-r>"]<esc>')

-- Disable linenumber in terminal
vim.api.nvim_create_autocmd("TermOpen", {
  desc = "Disable linenumber in terminal",
  callback = function ()
    vim.opt_local.number = false
  end
})

-- Highlight when yanking (copying) text
--  Try it with `yap` in normal mode
--  See `:help vim.highlight.on_yank()`
vim.api.nvim_create_autocmd('TextYankPost', {
  desc = 'Highlight when yanking (copying) text',
  group = vim.api.nvim_create_augroup('kickstart-highlight-yank', { clear = true }),
  callback = function()
    vim.highlight.on_yank()
  end,
})

-- Saves the current file into a register
function SaveFileToRegister()
  local file_name = vim.fn.expand("%:.")
  local register = vim.fn.input("Enter register key: ")
  vim.cmd("let @"..register.." = \":e "..file_name.."\"")
end

vim.keymap.set('n', '<leader>fr', SaveFileToRegister, { desc = "Save [F]ile to [R]egister" } )

-- Allow creating nested file
vim.api.nvim_create_user_command("MkdirAndSave",
  function ()
    vim.cmd("silent !mkdir -p %:h")
    vim.cmd.write()
  end,
{})

vim.keymap.set('n', '<leader>w', '<cmd>MkdirAndSave<CR>', { desc = '[W]rite nested file' })

-- Toggle terminal
function ToggleTerminal()
  local buffer_list = vim.api.nvim_list_bufs()
  local terminal_exists = false
  for _, buffer in ipairs(buffer_list) do
    local buffer_type = vim.api.nvim_buf_get_option(buffer, 'buftype')
    if buffer_type == "terminal" then
      if vim.fn.bufwinnr(buffer) < 0 then
        vim.cmd.split()
        vim.cmd.b(buffer)
        vim.cmd.norm("i")
      else
        vim.cmd("execute "..vim.fn.bufwinnr(buffer).." \"close\"")
      end
      terminal_exists = true
      break
    end
  end
  if terminal_exists == false then
    vim.cmd.split()
    vim.cmd.terminal()
    vim.cmd.norm("i")
  end
end

vim.keymap.set({'n', 't'}, '<M-t>', ToggleTerminal)

-- Plugins
local lazypath = vim.fn.stdpath 'data' .. '/lazy/lazy.nvim'
if not vim.loop.fs_stat(lazypath) then
  local lazyrepo = 'https://github.com/folke/lazy.nvim.git'
  vim.fn.system { 'git', 'clone', '--filter=blob:none', '--branch=stable', lazyrepo, lazypath }
end ---@diagnostic disable-next-line: undefined-field
vim.opt.rtp:prepend(lazypath)

require('lazy').setup({
  'tpope/vim-sleuth', -- Detect tabstop and shiftwidth automatically

  {
    "rose-pine/neovim",
    priority = 1000,
    name = "rose-pine",
    config = function ()
      vim.cmd.colorscheme("rose-pine")
    end
  },

  {
    'lewis6991/gitsigns.nvim',
    opts = {
      signs = {
        add = { text = '+' },
        change = { text = '~' },
        delete = { text = '_' },
        topdelete = { text = '‾' },
        changedelete = { text = '~' },
      },
    },
    config = function ()
      require 'gitsigns'.setup {}
      vim.keymap.set('n', '<leader>gb', '<cmd>Gitsigns blame_line<CR>', { desc = '[G]it [B]lame current line' })
    end
  },

  {
    'nvim-lualine/lualine.nvim',
    config = function ()
      require 'lualine'.setup({
        options = {
          section_separators = ""
        },
        sections = {
          lualine_c = {
            { 'filename', path = 1 }
          }
        }
      })
    end
  },

  {
    'nvim-telescope/telescope.nvim',
    event = 'VimEnter',
    branch = '0.1.x',
    dependencies = {
      'nvim-lua/plenary.nvim',
      {
        'nvim-telescope/telescope-fzf-native.nvim',
        build = 'make',
        cond = function()
          return vim.fn.executable 'make' == 1
        end,
      },
      { 'nvim-telescope/telescope-ui-select.nvim' },
    },
    config = function()
      -- Two important keymaps to use while in Telescope are:
      --  - Insert mode: <c-/>
      --  - Normal mode: ?
      require('telescope').setup {
        extensions = {
          ['ui-select'] = {
            require('telescope.themes').get_dropdown(),
          },
        },
      }

      -- Enable Telescope extensions if they are installed
      pcall(require('telescope').load_extension, 'fzf')
      pcall(require('telescope').load_extension, 'ui-select')

      -- See `:help telescope.builtin`
      local builtin = require 'telescope.builtin'
      vim.keymap.set('n', '<leader>fh', builtin.help_tags, { desc = '[S]earch [H]elp' })
      vim.keymap.set('n', '<leader>fk', builtin.keymaps, { desc = '[S]earch [K]eymaps' })
      vim.keymap.set('n', '<leader>ff', builtin.find_files, { desc = '[S]earch [F]iles' })
      vim.keymap.set('n', '<leader>fs', builtin.builtin, { desc = '[S]earch [S]elect Telescope' })
      vim.keymap.set('n', '<leader>fw', builtin.grep_string, { desc = '[S]earch current [W]ord' })
      vim.keymap.set('n', '<leader>fg', builtin.live_grep, { desc = '[S]earch by [G]rep' })
      vim.keymap.set('n', '<leader>fd', builtin.diagnostics, { desc = '[S]earch [D]iagnostics' })
      -- vim.keymap.set('n', '<leader>fr', builtin.resume, { desc = '[S]earch [R]esume' })
      vim.keymap.set('n', '<leader>f.', builtin.oldfiles, { desc = '[S]earch Recent Files ("." for repeat)' })
      vim.keymap.set('n', '<leader>fb', builtin.buffers, { desc = '[ ] Find existing buffers' })

      vim.keymap.set('n', '<leader>/', function()
        builtin.current_buffer_fuzzy_find(require('telescope.themes').get_dropdown {
          winblend = 10,
          previewer = false,
        })
      end, { desc = '[/] Fuzzily search in current buffer' })

      vim.keymap.set('n', '<leader>f/', function()
        builtin.live_grep {
          grep_open_files = true,
          prompt_title = 'Live Grep in Open Files',
        }
      end, { desc = '[S]earch [/] in Open Files' })

      vim.keymap.set('n', '<leader>fn', function()
        builtin.find_files { cwd = vim.fn.stdpath 'config' }
      end, { desc = '[S]earch [N]eovim files' })
    end,
  },

  {
    'neovim/nvim-lspconfig',
    dependencies = {
      'williamboman/mason.nvim',
      'williamboman/mason-lspconfig.nvim',
      'WhoIsSethDaniel/mason-tool-installer.nvim',

      -- Useful status updates for LSP.
      { 'j-hui/fidget.nvim', opts = {} },

      -- `neodev` configures Lua LSP for your Neovim config, runtime and plugins
      -- used for completion, annotations and signatures of Neovim apis
      { 'folke/neodev.nvim', opts = {} },
    },
    config = function()
      vim.api.nvim_create_autocmd('LspAttach', {
        group = vim.api.nvim_create_augroup('kickstart-lsp-attach', { clear = true }),
        callback = function(event)
          local map = function(keys, func, desc)
            vim.keymap.set('n', keys, func, { buffer = event.buf, desc = 'LSP: ' .. desc })
          end

          -- Jump to the definition of the word under your cursor.
          --  To jump back, press <C-t>.
          map('<leader>gd', require('telescope.builtin').lsp_definitions, '[G]oto [D]efinition')

          -- Find references for the word under your cursor.
          map('<leader>gr', require('telescope.builtin').lsp_references, '[G]oto [R]eferences')

          -- Jump to the implementation of the word under your cursor.
          --  Useful when your language has ways of declaring types without an actual implementation.
          map('<leader>gI', require('telescope.builtin').lsp_implementations, '[G]oto [I]mplementation')

          -- Jump to the type of the word under your cursor.
          --  Useful when you're not sure what type a variable is and you want to see
          --  the definition of its *type*, not where it was *defined*.
          map('<leader>D', require('telescope.builtin').lsp_type_definitions, 'Type [D]efinition')

          -- Fuzzy find all the symbols in your current document.
          --  Symbols are things like variables, functions, types, etc.
          map('<leader>ds', require('telescope.builtin').lsp_document_symbols, '[D]ocument [S]ymbols')

          -- Fuzzy find all the symbols in your current workspace.
          --  Similar to document symbols, except searches over your entire project.
          map('<leader>ws', require('telescope.builtin').lsp_dynamic_workspace_symbols, '[W]orkspace [S]ymbols')

          -- Rename the variable under your cursor.
          --  Most Language Servers support renaming across files, etc.
          map('<leader>rn', vim.lsp.buf.rename, '[R]e[n]ame')

          -- Execute a code action, usually your cursor needs to be on top of an error
          -- or a suggestion from your LSP for this to activate.
          map('<leader>ca', vim.lsp.buf.code_action, '[C]ode [A]ction')

          -- Opens a popup that displays documentation about the word under your cursor
          --  See `:help K` for why this keymap.
          map('K', vim.lsp.buf.hover, 'Hover Documentation')

          -- Provides signature help
          vim.keymap.set('i', '<C-k>', vim.lsp.buf.signature_help, { desc = 'Signature Help' })

          -- WARN: This is not Goto Definition, this is Goto Declaration.
          --  For example, in C this would take you to the header.
          map('<leader>gD', vim.lsp.buf.declaration, '[G]oto [D]eclaration')
        end,
      })

      local capabilities = vim.lsp.protocol.make_client_capabilities()
      capabilities = vim.tbl_deep_extend('force', capabilities, require('cmp_nvim_lsp').default_capabilities())

      local servers = {
        pyright = {},
        rust_analyzer = {},
        tsserver = {},
        eslint = {
          mapping = {
            vim.keymap.set('n', '<leader>fe', '<cmd>EslintFixAll<CR><cmd>w<CR>', { desc = '[F]ormat with [E]slint' })
          }
        },
        lua_ls = {
          settings = {
            Lua = {
              completion = {
                callSnippet = 'Replace',
              },
              -- You can toggle below to ignore Lua_LS's noisy `missing-fields` warnings
              -- diagnostics = { disable = { 'missing-fields' } },
            },
          },
        },
      }

      require('mason').setup()

      -- You can add other tools here that you want Mason to install
      -- for you, so that they are available from within Neovim.
      local ensure_installed = vim.tbl_keys(servers or {})

      require('mason-tool-installer').setup { ensure_installed = ensure_installed }

      require('mason-lspconfig').setup {
        handlers = {
          function(server_name)
            local server = servers[server_name] or {}
            -- This handles overriding only values explicitly passed
            -- by the server configuration above. Useful when disabling
            -- certain features of an LSP (for example, turning off formatting for tsserver)
            server.capabilities = vim.tbl_deep_extend('force', {}, capabilities, server.capabilities or {})
            require('lspconfig')[server_name].setup(server)
          end,
        },
      }

      vim.keymap.set('n', '<leader>lx', '<cmd>LspRestart<CR>', { desc = 'Restart the LSP servers' })
    end,
  },

  {
    'hrsh7th/nvim-cmp',
    event = 'InsertEnter',
    dependencies = {
      {
        'L3MON4D3/LuaSnip',
        build = (function()
          -- Build Step is needed for regex support in snippets.
          -- This step is not supported in many windows environments.
          -- Remove the below condition to re-enable on windows.
          if vim.fn.has 'win32' == 1 or vim.fn.executable 'make' == 0 then
            return
          end
          return 'make install_jsregexp'
        end)(),
        config = function()
          local ls = require("luasnip")
          local snip = ls.snippet
          local text = ls.text_node
          local insert = ls.insert_node
          local func = ls.function_node

          local function copy(args)
            return args[1]
          end

          local function react_snippets(filetype)
            ls.add_snippets(filetype, {
              snip('rfc', {
                text({'import React from \'react\';', '', ''}),
                text('export function '), insert(1, 'ComponentName'), text({'() {', ''}),
                text({'\treturn (', ''}),
                text('\t\t<div>'), insert(2, 'body'), text({'</div>', ''}),
                text({'\t);', ''}),
                text('}'),
              }),
              snip('rafc', {
                text({'import React from \'react\';', '', ''}),
                text('const '), insert(1, 'ComponentName'), text({' = () => {', ''}),
                text({'\treturn (', ''}),
                text('\t\t<div>'), insert(2, 'body'), text({'</div>', ''}),
                text({'\t);', ''}),
                text({'};', '', ''}),
                text('export default '), func(copy, 1), text(';')
              }),
              snip('sta', {
                text('const ['), insert(1, 'state'), text(', '),
                insert(2, 'setState'), text('] = useState('), insert(3, '\'initialValue\''), text(');'),
              }),
              snip('img', {
                text({'<img src="'}), insert(1), text({'" alt="'}), insert(2), text({'" />'})
              }),
            })
          end

          local function html_jsx_common_snippets(filetype)
            local snips = {
              snip('div', {
                text({'<div>'}), insert(1), text({'</div>'})
              }),
              snip('p', {
                text({'<p>'}), insert(1), text({'</p>'})
              }),
              snip('button', {
                text({'<button type="'}), insert(2, "button"), text({'">'}), insert(1), text({'</button>'})
              }),
              snip('title', {
                text({'<title>'}), insert(1), text({'</title>'})
              }),
            }
            for i = 1,6 do
              table.insert(snips, snip('h'..i, {
                text({'<h'..i..'>'}), insert(1), text({'</h'..i..'>'})
              }))
            end
            ls.add_snippets(filetype, snips)
          end

          local function html_snippets()
            ls.add_snippets('html', {
              snip('html', {
                text({'<!DOCTYPE html>', ''}),
                text({'<html>', ''}),
                text({'<head>', ''}),
                text({'\t<meta charset="UTF-8">', ''}),
                text({'\t<meta name="description" content="A nice description">', ''}),
                text({'\t<meta name="keywords" content="HTML, CSS, JavaScript">', ''}),
                text({'\t<meta name="viewport" content="width=device-width, initial-scale=1.0">', ''}),
                text('\t<title>'), insert(2, 'Title'), text({'</title>', ''}),
                text({'</head>', ''}),
                text({'<body>', ''}),
                text('\t'), insert(1, 'body'),
                text({'', '</body>', ''}),
                text('</html>'),
              }),
              snip('img', {
                text({'<img src="'}), insert(1), text({'" alt="'}), insert(2), text({'">'})
              }),
            })
          end

          local react_filetypes = {'javascriptreact', 'typescriptreact'}

          for _, filetype in ipairs(react_filetypes) do
            react_snippets(filetype)
          end

          local html_jsx_filetypes = {'javascriptreact', 'typescriptreact', 'html'}

          for _, filetype in ipairs(html_jsx_filetypes) do
            html_jsx_common_snippets(filetype)
          end

          html_snippets()
        end
      },
      'saadparwaiz1/cmp_luasnip',
      'hrsh7th/cmp-nvim-lsp',
      'hrsh7th/cmp-path',
      'hrsh7th/cmp-buffer',
    },

    config = function()
      local cmp = require 'cmp'
      local luasnip = require 'luasnip'
      luasnip.config.setup {}

      local source_names = {
        buffer = "[Buffer]",
        nvim_lsp = "[LSP]",
        luasnip = "[LuaSnip]",
        nvim_lua = "[Lua]",
        latex_symbols = "[LaTeX]",
      }

      cmp.setup {
        snippet = {
          expand = function(args)
            luasnip.lsp_expand(args.body)
          end,
        },
        ---@diagnostic disable-next-line: missing-fields
        formatting = {
          format = function(entry, vim_item)
            vim_item.menu = source_names[entry.source.name]
            return vim_item
          end
        },
        completion = { completeopt = 'menu,menuone,noselect' },
        mapping = cmp.mapping.preset.insert {
          ['<C-n>'] = cmp.mapping.select_next_item(),
          ['<C-p>'] = cmp.mapping.select_prev_item(),
          ['<C-y>'] = cmp.mapping.confirm { select = true },

          ['<C-b>'] = cmp.mapping.scroll_docs(-4),
          ['<C-f>'] = cmp.mapping.scroll_docs(4),
          ['<C-l>'] = cmp.mapping(function()
            if luasnip.expand_or_locally_jumpable() then
              luasnip.expand_or_jump()
            end
          end, { 'i', 's' }),
          ['<C-h>'] = cmp.mapping(function()
            if luasnip.locally_jumpable(-1) then
              luasnip.jump(-1)
            end
          end, { 'i', 's' }),
        },
        sources = {
          { name = 'nvim_lsp' },
          { name = 'luasnip' },
          { name = 'path' },
          { name = 'buffer' },
        },
      }
    end,
  },

  { 'folke/todo-comments.nvim', event = 'VimEnter', dependencies = { 'nvim-lua/plenary.nvim' }, opts = { signs = false } },

  {
    'nvim-treesitter/nvim-treesitter',
    build = ':TSUpdate',
    opts = {
      ensure_installed = { 'lua', 'markdown', 'javascript', 'vimdoc', 'typescript', 'python', 'rust' },
      highlight = {
        enable = true,
        additional_vim_regex_highlighting = false,
      },
      indent = { enable = true },
    },
    config = function(_, opts)
      require('nvim-treesitter.configs').setup(opts)
    end,
  },

  { 'skywind3000/vim-color-export' },

  {
    'Exafunction/codeium.vim',
    event = 'BufEnter'
  },
})
