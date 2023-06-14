return {
  {
    'Wansmer/treesj',
    dependencies = { 'nvim-treesitter/nvim-treesitter' },
    config = function()
      require('treesj').setup({
        use_default_keymaps = false,
        notify = false,
      })
    end,
  },
  {
    'windwp/nvim-autopairs',
    event = "InsertEnter",
    opts = {}
  },
  { 'ledger/vim-ledger',  enabled = false },
  { 'jpalardy/vim-slime', enabled = false },
  'neovim/nvim-lspconfig',
  'JoosepAlviste/nvim-ts-context-commentstring',
  'kg8m/vim-simple-align',
  {
    "SmiteshP/nvim-navic",
    dependencies = "neovim/nvim-lspconfig"
  },
  {
    'numToStr/Comment.nvim',
    config = function()
      require('Comment').setup()
    end,
  },
  {
    'uga-rosa/ccc.nvim',
    config = function()
      require('ccc').setup({
        empty_point_bg = false,
      })
    end,
  },
  { 'hrsh7th/nvim-cmp' },
  {
    'hrsh7th/cmp-buffer',
    'hrsh7th/cmp-nvim-lsp',
    'hrsh7th/cmp-path',
    'dcampos/cmp-snippy',
    'hrsh7th/cmp-nvim-lsp-signature-help',
    dependencies = { 'hrsh7th/nvim-cmp' },
  },
  {
    'dcampos/nvim-snippy',
    keys = {
    },
    config = function()
      require('snippy').setup({
        mappings = {
          i = {
            ["<C-j>"] = "expand_or_advance",
            ["<C-k>"] = "previous",
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
          map('v', '<leader>hs', function() gs.stage_hunk {vim.fn.line('.'), vim.fn.line('v')} end)
          map('v', '<leader>hr', function() gs.reset_hunk {vim.fn.line('.'), vim.fn.line('v')} end)
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
    }
  },
  { 'nvim-telescope/telescope-fzf-native.nvim', build = 'cmake -S. -Bbuild -DCMAKE_BUILD_TYPE=Release && cmake --build build --config Release && cmake --install build --prefix build' },
  { 'nvim-telescope/telescope-ui-select.nvim' },
  'jose-elias-alvarez/null-ls.nvim',
  {
    'jose-elias-alvarez/typescript.nvim',
    config = function()
      require('typescript').setup({})
    end
  },
  'Hoffs/omnisharp-extended-lsp.nvim',
  { 'nvim-treesitter/nvim-treesitter', build = ':TSUpdate' },
  'nvim-treesitter/nvim-treesitter-refactor',
  'nvim-treesitter/nvim-treesitter-textobjects',
  'nvim-treesitter/playground',
  {
    "nvim-telescope/telescope-file-browser.nvim",
    dependencies = { "nvim-telescope/telescope.nvim", "nvim-lua/plenary.nvim" },
  },
  {
    'stevearc/oil.nvim',
    config = function()
      require('oil').setup({
        view_options = {
          show_hidden = true,
        },
        keymaps = {
          ["!"] = "actions.open_terminal",
          ["."] = "actions.open_cmdline",
        },
      })
    end
  },
  'chrisbra/unicode.vim',
  'preservim/vim-markdown',
  'tpope/vim-abolish',
  {
    "kylechui/nvim-surround",
    event = "VeryLazy",
    config = function()
      require("nvim-surround").setup()
    end
  },
  'tpope/vim-unimpaired',
  'tpope/vim-sleuth',
  'tpope/vim-fugitive',
  'tpope/vim-rhubarb',
  'tpope/vim-rsi',
  'tpope/vim-eunuch',
  'lewis6991/impatient.nvim',
  { 'mcchrish/zenbones.nvim', dependencies = 'rktjmp/lush.nvim', enabled = false },
  { dir = '~/code/ploy.nvim', dependencies = 'rktjmp/lush.nvim' },
  {
    'aktersnurra/no-clown-fiesta.nvim',
    dependencies = 'rktjmp/lush.nvim',
    enabled = false,
    config = function()
      require('no-clown-fiesta').setup({
        styles = {
          comments = { italic = true },
        },
      })
    end,
  },
  {
    'rktjmp/paperplanes.nvim',
    config = function()
      require('paperplanes').setup({
        provider = 'paste.rs',
      })
    end
  },
  {
    'alaviss/nim.nvim',
    enabled = false,
  },
  'nvim-tree/nvim-web-devicons',
  { 'j-hui/fidget.nvim', tag = "legacy", config = function() require('fidget').setup() end },
}
