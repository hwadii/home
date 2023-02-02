return {
  'wbthomason/packer.nvim',
  'AndrewRadev/splitjoin.vim',
  {
    'windwp/nvim-autopairs',
    config = function()
      require('nvim-autopairs').setup()
    end
  },
  'ledger/vim-ledger',
  'jpalardy/vim-slime',
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
  'brenoprata10/nvim-highlight-colors',
  { 'hrsh7th/nvim-cmp' },
  {
    'hrsh7th/cmp-buffer',
    'hrsh7th/cmp-nvim-lsp',
    'hrsh7th/cmp-path',
    'saadparwaiz1/cmp_luasnip',
    'hrsh7th/cmp-nvim-lsp-signature-help',
    dependencies = { 'hrsh7th/nvim-cmp' },
  },
  'L3MON4D3/LuaSnip',
  'lewis6991/github_dark.nvim',
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
          map({ 'n', 'v' }, '<leader>hs', gs.stage_hunk)
          map({ 'n', 'v' }, '<leader>hr', gs.reset_hunk)
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
  { 'nvim-telescope/telescope-fzf-native.nvim', build = 'make' },
  { 'nvim-telescope/telescope-ui-select.nvim' },
  'jose-elias-alvarez/null-ls.nvim',
  {
    'jose-elias-alvarez/typescript.nvim',
    config = function()
      require('typescript').setup({})
    end
  },
  { 'nvim-treesitter/nvim-treesitter', build = ':TSUpdate' },
  'nvim-treesitter/nvim-treesitter-refactor',
  -- { 'nvim-treesitter/nvim-treesitter-angular' }
  'nvim-treesitter/nvim-treesitter-textobjects',
  'nvim-treesitter/playground',
  'chrisbra/unicode.vim',
  'preservim/vim-markdown',
  'tpope/vim-abolish',
  {
    'kylechui/nvim-surround',
    config = function()
      require('nvim-surround').setup()
    end
  },
  'tpope/vim-unimpaired',
  'tpope/vim-sleuth',
  'tpope/vim-fugitive',
  'tpope/vim-rhubarb',
  'tpope/vim-rsi',
  'tpope/vim-eunuch',
  'justinmk/vim-dirvish',
  'lewis6991/impatient.nvim',
  { 'mcchrish/zenbones.nvim', dependencies = 'rktjmp/lush.nvim' },
  { dir = '~/code/ploy.nvim', dependencies = 'rktjmp/lush.nvim' },
  {
    'rktjmp/paperplanes.nvim',
    config = function()
      require('paperplanes').setup({
        provider = 'paste.rs',
      })
    end
  },
  'alaviss/nim.nvim',
  { 'j-hui/fidget.nvim', config = function() require('fidget').setup() end },
}
