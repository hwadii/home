local fn = vim.fn
local packer = require('packer')
local util = require('packer.util')

local install_path = fn.stdpath('data')..'/site/pack/packer/start/packer.nvim'
if fn.empty(fn.glob(install_path)) > 0 then
  packer_bootstrap = fn.system({'git', 'clone', '--depth', '1', 'https://github.com/wbthomason/packer.nvim', install_path})
end

return packer.startup({function()
  use 'wbthomason/packer.nvim'

  use 'AndrewRadev/splitjoin.vim'
  use 'airblade/vim-rooter'
  use 'jiangmiao/auto-pairs'
  use 'lambdalisue/gina.vim'
  use 'ledger/vim-ledger'
  use 'wlangstroth/vim-racket'
  use 'jpalardy/vim-slime'
  use 'neovim/nvim-lspconfig'
  use 'JoosepAlviste/nvim-ts-context-commentstring'
  use {
    'numToStr/Comment.nvim',
    config = function() require('Comment').setup() end,
  }
  use {
    'norcalli/nvim-colorizer.lua',
    config = function() require('colorizer').setup({}, { names = false }) end,
  }
  use {
    'hrsh7th/nvim-cmp',
    requires = {
      'hrsh7th/cmp-buffer',
      'hrsh7th/cmp-nvim-lsp',
      'hrsh7th/cmp-path',
      'saadparwaiz1/cmp_luasnip'
    }
  }
  use 'L3MON4D3/LuaSnip'
  use 'hwadii/github_dark.nvim'
  use {
    'lewis6991/gitsigns.nvim',
    requires = {
      'nvim-lua/plenary.nvim'
    },
    config = function() require('gitsigns').setup() end,
  }
  use {
    'nvim-telescope/telescope.nvim',
    requires = {
      'nvim-lua/plenary.nvim',
      'nvim-telescope/telescope-fzy-native.nvim'
    }
  }
  use 'jose-elias-alvarez/null-ls.nvim'
  use 'jose-elias-alvarez/nvim-lsp-ts-utils'
  use { 'nvim-treesitter/nvim-treesitter', run = ':TSUpdate' }
  use 'nvim-treesitter/nvim-treesitter-refactor'
  use 'nvim-treesitter/nvim-treesitter-angular'
  use 'nvim-treesitter/nvim-treesitter-textobjects'
  use 'chrisbra/unicode.vim'
  use 'plasticboy/vim-markdown'
  use 'tpope/vim-abolish'
  use 'tpope/vim-surround'
  use 'tpope/vim-unimpaired'
  use 'tpope/vim-sleuth'
  use 'justinmk/vim-dirvish'
  use 'lewis6991/impatient.nvim'
  use 'tpope/vim-rsi'
  use 'rktjmp/lush.nvim'
  use '~/code/gruber_darker.nvim'
  use 'mcchrish/zenbones.nvim'

  if packer_bootstrap then
    require('packer').sync()
  end
end, config = { compile_path = util.join_paths(vim.fn.stdpath('config'), 'lua', 'packer_compiled.lua'), }
})
