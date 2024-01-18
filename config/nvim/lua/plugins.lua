return {
  {
    'Wansmer/treesj',
    dependencies = { 'nvim-treesitter/nvim-treesitter' },
    opts = {
      use_default_keymaps = false,
      notify = false,
    },
    keys = {
      { 'gS', '<cmd>TSJToggle<cr>', desc = 'Join Toggle', mode = 'n' },
    },
  },
  {
    'windwp/nvim-autopairs',
    event = 'InsertEnter',
    opts = {
      fast_wrap = {},
    }
  },
  {
    'ledger/vim-ledger',
    enabled = false,
    init = function()
      vim.g.ledger_align_at = 52
    end
  },
  {
    'jpalardy/vim-slime',
    enabled = false,
    init = function()
      vim.g.slime_target = 'tmux'
      vim.g.slime_paste_file = '/tmp/.slime_paste'
      vim.g.slime_default_config = { socket_name = "default", target_pane = "{last}" }
      vim.g.slime_dont_ask_default = 1
      vim.g.slime_no_mappings = 1
    end
  },
  {
    'neovim/nvim-lspconfig',
    event = { 'BufReadPre', 'BufNewFile' },
    opts = {
      diagnostics = {
        underline = true,
        signs = false,
        update_in_insert = false,
        virtual_text = {
          prefix = '■',
          spacing = 4,
          source = 'if_many',
        },
        severity_sort = true,
      },
      capabilities = {},
    },
    config = function(_, opts)
      require('wadii.lsp')
      vim.diagnostic.config(vim.deepcopy(opts.diagnostics))
      vim.fn.sign_define('DiagnosticSignError', { text = '×', texthl = 'DiagnosticSignError' })
      vim.fn.sign_define('DiagnosticSignWarn', { text = '!', texthl = 'DiagnosticSignWarn' })
      vim.fn.sign_define('DiagnosticSignHint', { text = 'i', texthl = 'DiagnosticSignHint' })
      vim.fn.sign_define('DiagnosticSignInfo', { text = 'H', texthl = 'DiagnosticInfo' })
    end
  },
  { 'godlygeek/tabular', cmd = 'Tabularize' },
  {
    'SmiteshP/nvim-navic',
    dependencies = 'neovim/nvim-lspconfig',
    init = function()
      vim.g.navic_silence = 1
    end,
    enabled = false,
  },
  {
    'numToStr/Comment.nvim',
    config = function()
      require('Comment').setup({
        pre_hook = require('ts_context_commentstring.integrations.comment_nvim').create_pre_hook(),
      })
    end,
    event = "BufReadPre",
    dependencies = 'JoosepAlviste/nvim-ts-context-commentstring',
  },
  {
    'JoosepAlviste/nvim-ts-context-commentstring',
    opts = {
      enable_autocmd = false,
    }
  },
  {
    'uga-rosa/ccc.nvim',
    opts = { empty_point_bg = false },
    cmd = { 'CccHighlighterToggle', 'CccPick' },
    keys = {
      { '<leader>cc', '<cmd>CccHighlighterToggle<cr>', mode = 'n' },
      { '<leader>cp', '<cmd>CccPick<cr>', mode = 'n' },
    },
  },
  {
    'hrsh7th/nvim-cmp',
    event = 'InsertEnter',
    dependencies = {
      'hrsh7th/cmp-buffer',
      'hrsh7th/cmp-nvim-lsp',
      'hrsh7th/cmp-path',
      'hrsh7th/cmp-emoji',
      'dcampos/cmp-snippy',
      'hrsh7th/cmp-nvim-lsp-signature-help',
    },
    opts = function(_, opts)
      local cmp = require('cmp')
      opts.snippet = {
        expand = function(args)
          require('snippy').expand_snippet(args.body)
        end
      }
      opts.mapping = cmp.mapping.preset.insert({
        ['<C-b>'] = cmp.mapping.scroll_docs(-4),
        ['<C-f>'] = cmp.mapping.scroll_docs(4),
        ['<C-e>'] = cmp.mapping.abort(),
        ['<CR>'] = cmp.mapping.confirm({ select = true }),
      })
      opts.window = {
        documentation = cmp.config.window.bordered({
          winhighlight = "Normal:Normal,FloatBorder:Normal,CursorLine:Visual,Search:None"
        }),
      }
      opts.sources = cmp.config.sources({
        { name = "nvim_lsp" },
        { name = "copilot" },
        { name = "vim-dadbod-completion" },
        { name = "snippy" },
        { name = "buffer", max_item_count = 10 },
        { name = "path" },
        { name = "emoji" },
      })
      opts.experimental = {
        ghost_text = false,
      }
    end,
  },
  {
    'dcampos/nvim-snippy',
    event = 'BufReadPre',
    config = function()
      require('snippy').setup({
        mappings = {
          i = {
            ['<C-j>'] = 'expand_or_advance',
            ['<C-k>'] = 'previous',
          },
        },
      })
    end
  },
  { 'lewis6991/github_dark.nvim', enabled = false },
  {
    'lewis6991/gitsigns.nvim',
    dependencies = {
      'nvim-lua/plenary.nvim'
    },
    event = 'BufReadPre',
    config = function()
      require('gitsigns').setup({
        on_attach = function(bufnr)
          local gs = package.loaded.gitsigns

          local function map(mode, l, r, opts)
            opts = opts or {}
            opts.buffer = bufnr
            vim.keymap.set(mode, l, r, opts)
          end

          -- Navigation
          map({ 'n', 'v' }, ']c', function()
            if vim.wo.diff then
              return ']c'
            end
            vim.schedule(function()
              gs.next_hunk()
            end)
            return '<Ignore>'
          end, { expr = true, desc = 'Jump to next hunk' })

          map({ 'n', 'v' }, '[c', function()
            if vim.wo.diff then
              return '[c'
            end
            vim.schedule(function()
              gs.prev_hunk()
            end)
            return '<Ignore>'
          end, { expr = true, desc = 'Jump to previous hunk' })

          -- Actions
          -- visual mode
          map('v', '<leader>hs', function()
            gs.stage_hunk { vim.fn.line '.', vim.fn.line 'v' }
          end, { desc = 'stage git hunk' })
          map('v', '<leader>hr', function()
            gs.reset_hunk { vim.fn.line '.', vim.fn.line 'v' }
          end, { desc = 'reset git hunk' })
          -- normal mode
          map('n', '<leader>hs', gs.stage_hunk, { desc = 'git stage hunk' })
          map('n', '<leader>hr', gs.reset_hunk, { desc = 'git reset hunk' })
          map('n', '<leader>hS', gs.stage_buffer, { desc = 'git Stage buffer' })
          map('n', '<leader>hu', gs.undo_stage_hunk, { desc = 'undo stage hunk' })
          map('n', '<leader>hR', gs.reset_buffer, { desc = 'git Reset buffer' })
          map('n', '<leader>hp', gs.preview_hunk, { desc = 'preview git hunk' })
          map('n', '<leader>hb', function()
            gs.blame_line { full = false }
          end, { desc = 'git blame line' })
          map('n', '<leader>hd', gs.diffthis, { desc = 'git diff against index' })
          map('n', '<leader>hD', function()
            gs.diffthis '~'
          end, { desc = 'git diff against last commit' })

          -- Toggles
          map('n', '<leader>tb', gs.toggle_current_line_blame, { desc = 'toggle git blame line' })
          map('n', '<leader>td', gs.toggle_deleted, { desc = 'toggle git show deleted' })

          -- Text object
          map({ 'o', 'x' }, 'ih', ':<C-U>Gitsigns select_hunk<CR>', { desc = 'select git hunk' })
        end,
      })
    end
  },
  {
    'nvim-telescope/telescope.nvim',
    dependencies = {
      'nvim-lua/plenary.nvim',
      'nvim-telescope/telescope-ui-select.nvim',
      'nvim-telescope/telescope-file-browser.nvim'
    },
    cmd = 'Telescope',
    keys = {
      { '<C-p>', '<cmd>Telescope find_files<cr>', { mode = 'n' } },
      { '<Leader>sf', '<cmd>Telescope find_files<cr>', { mode = 'n' } },
      { '<leader>:', '<cmd>Telescope command_history<cr>' },
      { '<Leader>?', '<cmd>Telescope builtin<cr>', { mode = 'n' } },
      { '<Leader>/', '<cmd>Telescope live_grep<cr>', { mode = 'n' } },
      { '<localleader><localleader>', '<cmd>Telescope buffers<cr>', { mode = 'n' } },
      { '<leader>sH', '<cmd>Telescope highlights<cr>' },
      { '<Leader>s?', '<cmd>Telescope oldfiles<cr>', { mode = 'n' } },
      { '<Leader>sc', '<cmd>Telescope git_commits<cr>', { mode = 'n' } },
      { '<Leader>se', '<cmd>Telescope resume<cr>', { mode = 'n' } },
      { '<Leader>s"', '<cmd>Telescope registers<cr>', { mode = 'n' } },
      { '<Leader>sd', '<cmd>Telescope diagnostics<cr>', { mode = 'n' } },
      { '<C-s>', '<cmd>Telescope current_buffer_fuzzy_find<cr>', { mode = 'n' } },
      {
        '<Leader>s-', function() require('telescope').extensions.file_browser.file_browser({ path = '%:p:h', select_buffer = true }) end,
      },
      { '<Leader>sw', function()
        vim.ui.input({ prompt = 'Grep For > ' }, function(input)
          require('telescope.builtin').grep_string({ search = input, word_match = '-w' })
        end)
      end },
      { '<Leader>sW', function() require('telescope.builtin').grep_string({ word_match = '-w' }) end },
    },
    config = function()
      local telescope = require('telescope')
      local actions = require('telescope.actions')
      local themes = require('telescope.themes')
      local action_state = require('telescope.actions.state')
      local action_layout = require('telescope.actions.layout')

      telescope.setup({
        defaults = {
          layout_strategy = 'flex',
          path_display = { 'shorten' },
          mappings = {
            i = {
              ['<C-s>'] = actions.select_horizontal,
              ['<C-f>'] = actions.preview_scrolling_right,
              ['<C-b>'] = actions.preview_scrolling_left,
              ['<a-l>'] = actions.smart_send_to_loclist + actions.open_loclist,
              ['<a-q>'] = actions.smart_send_to_qflist + actions.open_qflist,
              ['<esc>'] = actions.close,
              ['<C-[>'] = actions.close,
              ['<C-c>'] = actions.close,
              ['<M-p>'] = action_layout.toggle_preview
            },
            n = {
              ['<esc>'] = actions.close,
              ['<M-p>'] = action_layout.toggle_preview
            },
          },
        },
        pickers = {
          find_files = {
            theme = 'ivy',
            find_command = { 'fd', '--hidden', '-tf' },
            path_display = { 'truncate' },
            previewer = false,
          },
          fzf = {
            fuzzy = true,             -- false will only do exact matching
            override_generic_sorter = true, -- override the generic sorter
            override_file_sorter = true, -- override the file sorter
            case_mode = 'smart_case', -- or 'ignore_case' or 'respect_case'
          },
          lsp_references = { theme = 'dropdown' },
          lsp_definitions = { theme = 'dropdown' },
          lsp_implementations = { theme = 'dropdown' },
          lsp_code_actions = { theme = 'cursor' },
          code_action = { theme = 'cursor' },
          buffers = {
            path_display = { shorten = 3 },
            ignore_current_buffer = false,
            sort_mru = true,
            theme = 'dropdown',
            previewer = false,
            layout_config = {
              height = 20,
            },
            mappings = {
              i = {
                ['<c-d>'] = actions.delete_buffer,
                ['<a-d>'] = function(prompt_bufnr)
                  local current_picker = action_state.get_current_picker(prompt_bufnr)
                  current_picker:delete_selection(function(selection)
                    vim.api.nvim_buf_delete(selection.bufnr, { force = true })
                  end)
                end,
              }
            }
          },
          marks = {
            mappings = {
              i = {
                ['<a-d>'] = actions.delete_mark,
              }
            }
          }
        },
        extensions = {
          ['ui-select'] = {
            themes.get_cursor()
          },
          file_browser = {
            hijack_netrw = false,
            grouped = true,
          }
        }
      })
      telescope.load_extension('fzf')
      telescope.load_extension('ui-select')
      telescope.load_extension('file_browser')
    end
  },
  {
    'nvim-telescope/telescope-fzf-native.nvim',
    build = 'cmake -S. -Bbuild -DCMAKE_BUILD_TYPE=Release && cmake --build build --config Release && cmake --install build --prefix build',
  },
  { 'Hoffs/omnisharp-extended-lsp.nvim', ft = 'csharp' },
  { 'aklt/plantuml-syntax', ft = 'plantuml' },
  { 'terrastruct/d2-vim', ft = 'd2' },
  {
    'mhartington/formatter.nvim',
    config = function()
      require('formatter').setup({
        filetype = {
          ['*'] = require('formatter.filetypes.any').remove_trailing_whitespace,
          sql = require('formatter.filetypes.sql').pgformat,
          xml = require('formatter.filetypes.xml').xmllint,
          json = require('formatter.filetypes.json').jq,
        }
      })
    end,
    keys = {
      { '<leader>f', '<cmd>Format<cr>' },
      { '<leader>F', '<cmd>FormatWrite<cr>' },
    },
    cmd = { 'Format', 'FormatWrite' },
  },
  {
    'nvim-treesitter/nvim-treesitter',
    build = ':TSUpdate',
    dependencies = {
      'nvim-treesitter/nvim-treesitter-refactor',
      'nvim-treesitter/nvim-treesitter-textobjects',
    }
  },
  {
    'echasnovski/mini.files',
    keys = {
      { '-', "<cmd>lua require('wadii.various').toggle_minifiles(vim.api.nvim_buf_get_name(0))<cr>", mode = 'n', desc = 'Open parent directory in a buffer.' },
    },
  },
  {
    'echasnovski/mini.move',
    keys = {
      { '<M-k>', "<cmd>lua require('mini.move').move_line('up')<cr>", mode = 'n', desc = 'Move line up' },
      { '<M-j>', "<cmd>lua require('mini.move').move_line('down')<cr>", mode = 'n', desc = 'Move line down' },
      { '<M-l>', "<cmd>lua require('mini.move').move_line('right')<cr>", mode = 'n', desc = 'Move line right' },
      { '<M-h>', "<cmd>lua require('mini.move').move_line('left')<cr>", mode = 'n', desc = 'Move line left' },
      { '<M-k>', "<cmd>lua require('mini.move').move_selection('up')<cr>", mode = 'v', desc = 'Move selection up' },
      { '<M-j>', "<cmd>lua require('mini.move').move_selection('down')<cr>", mode = 'v', desc = 'Move selection down' },
      { '<M-l>', "<cmd>lua require('mini.move').move_selection('right')<cr>", mode = 'v', desc = 'Move selection right' },
      { '<M-h>', "<cmd>lua require('mini.move').move_selection('left')<cr>", mode = 'v', desc = 'Move selection left' },
    },
  },
  {
    'stevearc/oil.nvim',
    enabled = false,
    cmd = 'Oil',
    keys = {
      { '-', '<cmd>Oil<cr>', mode = 'n', desc = 'Open parent directory in a buffer.' },
    },
    opts = {
      view_options = {
        show_hidden = true,
      },
      buf_options = {
        buflisted = true,
        bufhidden = 'hide',
      },
      keymaps = {
        ['!'] = 'actions.open_terminal',
        ['.'] = 'actions.open_cmdline',
      },
    },
  },
  {
    'chrisbra/unicode.vim',
    cmd = { 'UnicodeSearch', 'Digraphs' },
  },
  {
    'preservim/vim-markdown',
    ft = 'markdown',
    init = function()
      vim.g.vim_markdown_override_foldtext = 0
      vim.g.vim_markdown_no_default_key_mappings = 1
      vim.g.vim_markdown_emphasis_multiline = 0
      vim.g.vim_markdown_conceal = 0
      vim.g.vim_markdown_conceal_code_blocks = 0
      vim.g.vim_markdown_frontmatter = 1
      vim.g.vim_markdown_borderless_table = 0
    end
  },
  {
    'kylechui/nvim-surround',
    event = 'VeryLazy',
    opts = {},
  },
  {
    'dstein64/nvim-scrollview',
    event = 'UIEnter',
    opts = {
      current_only = true,
      winblend = 40,
      signs_on_startup = { 'diagnostics', 'search', 'spell', 'marks', 'conflicts' },
      excluded_filetypes = { 'fugitiveblame' }
    }
  },
  {
    'zbirenbaum/copilot.lua',
    dependencies = { 'zbirenbaum/copilot-cmp' },
    cmd = 'Copilot',
    keys = {
      { '<leader>pe', '<cmd>Copilot enable<cr>', mode = 'n', desc = 'Copilot enable' },
      { '<leader>ps', '<cmd>Copilot status<cr>', mode = 'n', desc = 'Copilot status' },
    },
    build = ':Copilot auth',
    config = function()
      require('copilot').setup({
        suggestion = { enabled = false },
        panel = { enabled = false },
      })
    end,
  },
  {
    'zbirenbaum/copilot-cmp',
    config = function()
      require('copilot_cmp').setup()
    end
  },
  { 'tpope/vim-abolish', cmd = { 'Abolish', 'Subvert' }, keys = 'cr' },
  { 'tpope/vim-unimpaired', keys = { '[', ']', 'yo' } },
  { 'tpope/vim-sleuth', event = 'VeryLazy' },
  {
    'tpope/vim-fugitive',
    cmd = { 'Git', 'GBrowse' },
    keys = {
      { '<Leader>gs', '<cmd>Git<cr>', mode = 'n' },
      { '<Leader>gc', '<cmd>Git commit<cr>', mode = 'n' },
      { '<Leader>gp', '<cmd>Git push<cr>', mode = 'n' },
      { '<Leader>gl', '<cmd>Git log<cr>', mode = 'n' },
      { '<Leader>gb', '<cmd>Git blame<cr>', mode = 'n' },
      { '<Leader>ga', '<cmd>Git branch<cr>', mode = 'n' },
      { '<Leader>gr', '<cmd>Gr<cr>', { silent = true, mode = 'n' } },
    },
    dependencies = 'tpope/vim-rhubarb',
  },
  {
    'tpope/vim-rsi',
    event = { 'InsertEnter', 'CmdLineEnter' },
    init = function()
      vim.g.rsi_no_meta = 1
    end
  },
  { 'tpope/vim-eunuch', event = 'CmdLineEnter' },
  {
    'kristijanhusak/vim-dadbod-ui',
    dependencies = {
      { 'tpope/vim-dadbod', lazy = true },
      { 'kristijanhusak/vim-dadbod-completion', ft = { 'sql', 'mysql', 'plsql' }, lazy = true },
    },
    cmd = {
      'DBUI',
      'DBUIToggle',
      'DBUIAddConnection',
      'DBUIFindBuffer',
    },
    init = function()
      vim.g.db_ui_use_nerd_fonts = 1
    end,
  },
  { 'lewis6991/satellite.nvim', opts = {}, enabled = false },
  { 'mcchrish/zenbones.nvim', dependencies = 'rktjmp/lush.nvim', enabled = false },
  {
    dir = '~/code/ploy.nvim',
    dependencies = 'rktjmp/lush.nvim',
    init = function()
      vim.g.colors_name = 'ploy'
    end
  },
  {
    'rktjmp/paperplanes.nvim',
    opts = {
      provider = 'paste.rs',
    },
    cmd = 'PP',
  },
  { 'alaviss/nim.nvim', ft = 'nim' },
  {
    'pmizio/typescript-tools.nvim',
    dependencies = { 'nvim-lua/plenary.nvim', 'neovim/nvim-lspconfig' },
    opts = {},
    event = 'VeryLazy',
  },
  'nvim-tree/nvim-web-devicons',
  { 'j-hui/fidget.nvim', tag = 'legacy', opts = {}, event = 'BufReadPre' },
  {
    'sindrets/diffview.nvim',
    cmd = { 'DiffviewOpen', 'DiffviewClose', 'DiffviewToggleFiles', 'DiffviewFocusFiles' },
    config = true,
    keys = {
      { '<leader>gd', '<cmd>DiffviewOpen<cr>', desc = 'DiffviewOpen' },
      { '<leader>gh', '<cmd>DiffviewFileHistory %<cr>', desc = 'DiffviewFileHistory'}
    },
  },
  {
    'hedyhli/outline.nvim',
    cmd = { 'Outline', 'OutlineOpen' },
    keys = { -- Example mapping to toggle outline
      { '<leader>cs', '<cmd>Outline<CR>', desc = 'Toggle outline' },
    },
    opts = { { position = 'right' } },
  },
  {
    'danobi/prr',
    ft = 'prr',
    config = function(plugin)
      vim.opt.rtp:append(plugin.dir .. "/vim")
    end
  }
}
