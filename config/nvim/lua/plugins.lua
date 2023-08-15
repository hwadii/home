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
  { 'ledger/vim-ledger', enabled = false },
  { 'jpalardy/vim-slime', enabled = false },
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
      vim.fn.sign_define('DiagnosticSignWarn',  { text = '!', texthl = 'DiagnosticSignWarn' })
      vim.fn.sign_define('DiagnosticSignHint',  { text = 'i', texthl = 'DiagnosticSignHint' })
      vim.fn.sign_define('DiagnosticSignInfo',  { text = 'H', texthl = 'DiagnosticInfo' })
    end
  },
  { 'kg8m/vim-simple-align', cmd = 'SimpleAlign' },
  {
    'SmiteshP/nvim-navic',
    dependencies = 'neovim/nvim-lspconfig',
    event = 'UIEnter',
  },
  {
    'numToStr/Comment.nvim',
    config = function()
      require('Comment').setup({
        pre_hook = require('ts_context_commentstring.integrations.comment_nvim').create_pre_hook(),
      })
    end,
    lazy = false,
    dependencies = 'JoosepAlviste/nvim-ts-context-commentstring',
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
    event = 'BufReadPre',
    dependencies = {
      'hrsh7th/cmp-buffer',
      'hrsh7th/cmp-nvim-lsp',
      'hrsh7th/cmp-path',
      'dcampos/cmp-snippy',
      'hrsh7th/cmp-nvim-lsp-signature-help',
    }
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
          map('n', ']c', function()
            if vim.wo.diff then return ']c' end
            vim.schedule(function() gs.next_hunk() end)
            return '<Ignore>'
          end, { expr = true })

          map('n', '[c', function()
            if vim.wo.diff then return '[c' end
            vim.schedule(function() gs.prev_hunk() end)
            return '<Ignore>'
          end, { expr = true })

          -- Actions
          map('n', '<leader>hs', gs.stage_hunk)
          map('n', '<leader>hr', gs.reset_hunk)
          map('v', '<leader>hs', function() gs.stage_hunk { vim.fn.line('.'), vim.fn.line('v') } end)
          map('v', '<leader>hr', function() gs.reset_hunk { vim.fn.line('.'), vim.fn.line('v') } end)
          map('n', '<leader>hS', gs.stage_buffer)
          map('n', '<leader>hu', gs.undo_stage_hunk)
          map('n', '<leader>hR', gs.reset_buffer)
          map('n', '<leader>hp', gs.preview_hunk)
          map('n', '<leader>hb', function() gs.blame_line { full = true } end)
          map('n', '<leader>tb', gs.toggle_current_line_blame)
          map('n', '<leader>hd', gs.diffthis)
          map('n', '<leader>hD', function() gs.diffthis('~') end)
          map('n', '<leader>td', gs.toggle_deleted)

          -- Text object
          map({ 'o', 'x' }, 'ih', gs.select_hunk)
        end
      })
    end
  },
  {
    'nvim-telescope/telescope.nvim',
    dependencies = {
      'nvim-lua/plenary.nvim',
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
        '<Leader>s-',
        function() require('telescope').extensions.file_browser.file_browser({ path = '%:p:h', select_buffer = true }) end,
      },
      { '<Leader>sw', function() require('telescope.builtin').grep_string({ search = vim.fn.input('Grep For > '), word_match = '-w' }) end },
      { '<Leader>sW', function() require('telescope.builtin').grep_string({ word_match = '-w' }) end, mode = 'v' },
    },
    config = function()
      require('wadii.telescope')
    end
  },
  {
    'nvim-telescope/telescope-fzf-native.nvim',
    build = 'cmake -S. -Bbuild -DCMAKE_BUILD_TYPE=Release && cmake --build build --config Release && cmake --install build --prefix build',
  },
  { 'nvim-telescope/telescope-ui-select.nvim' },
  {
    'nvim-telescope/telescope-file-browser.nvim',
    dependencies = { 'nvim-telescope/telescope.nvim', 'nvim-lua/plenary.nvim' },
  },
  { 'Hoffs/omnisharp-extended-lsp.nvim', ft = 'csharp' },
  {
    'mhartington/formatter.nvim',
    config = function()
      require('formatter').setup({
        filetype = {
          ['*'] = require('formatter.filetypes.any').remove_trailing_whitespace,
          sql = require('formatter.filetypes.sql').pgformat,
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
    'nvim-treesitter/playground',
    event = 'VeryLazy',
    cmd = 'TSPlaygroundToggle',
  },
  {
    'stevearc/oil.nvim',
    cmd = 'Oil',
    keys = {
      { '-', '<cmd>Oil<cr>', mode = 'n', desc = 'Open parent directory in a buffer.' },
    },
    opts = {
      view_options = {
        show_hidden = true,
      },
      keymaps = {
        ['!'] = 'actions.open_terminal',
        ['.'] = 'actions.open_cmdline',
      },
    },
  },
  { 'chrisbra/unicode.vim', event = 'BufReadPre' },
  { 'preservim/vim-markdown', ft = 'markdown' },
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
      signs_on_startup = { 'diagnostics', 'search', 'spell' }
    }
  },
  { 'tpope/vim-abolish', cmd = { 'Abolish', 'Subvert' } },
  { 'tpope/vim-unimpaired', keys = { '[', ']' }, event = 'VeryLazy' },
  { 'tpope/vim-sleuth', event = 'VeryLazy' },
  {
    'tpope/vim-fugitive',
    cmd = { 'Git', 'GBrowse' },
    keys = {
      { '<Leader>gs', '<cmd>Git<cr>', mode = 'n' },
      { '<Leader>gc', '<cmd>Git commit<cr>', mode = 'n' },
      { '<Leader>gp', '<cmd>Git! push<cr>', mode = 'n' },
      { '<Leader>gl', '<cmd>Git log<cr>', mode = 'n' },
      { '<Leader>gb', '<cmd>Git blame<cr>', mode = 'n' },
      { '<Leader>ga', '<cmd>Git branch<cr>', mode = 'n' },
      { '<Leader>gr', '<cmd>Gr<cr>', { silent = true, mode = 'n' } },
    },
    dependencies = 'tpope/vim-rhubarb',
  },
  { 'tpope/vim-rsi', event = 'InsertEnter' },
  { 'tpope/vim-eunuch', event = 'CmdLineEnter' },
  { 'lewis6991/satellite.nvim', opts = {}, enabled = false },
  { 'mcchrish/zenbones.nvim', dependencies = 'rktjmp/lush.nvim', enabled = false },
  { dir = '~/code/ploy.nvim', dependencies = 'rktjmp/lush.nvim' },
  {
    'rktjmp/paperplanes.nvim',
    opts = {
      provider = '0x0.st',
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
    keys = { { '<leader>gd', '<cmd>DiffviewOpen<cr>', desc = 'DiffView' } },
  },
}
