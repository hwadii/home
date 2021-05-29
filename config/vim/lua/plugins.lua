vim.cmd [[packadd packer.nvim]]
local packer = require('packer')
local util = require('packer.util')

return packer.startup(function()
  use {'wbthomason/packer.nvim', opt = true}

  use 'AndrewRadev/splitjoin.vim'
  use 'airblade/vim-rooter'
  use 'junegunn/seoul256.vim'
  use 'axvr/org.vim'
  use 'jiangmiao/auto-pairs'
  use 'yorickpeterse/vim-paper'
  use 'lambdalisue/gina.vim'
  use 'ledger/vim-ledger'
  use 'justinmk/vim-dirvish'
  use 'neovim/nvim-lspconfig'
  use {
    'norcalli/nvim-colorizer.lua',
    config = function() require('colorizer').setup({}, { names = false }) end,
  }
  use 'hrsh7th/nvim-compe'
  use 'norcalli/snippets.nvim'
  use 'lewis6991/github_dark.nvim'
  use {
    'lewis6991/gitsigns.nvim',
    requires = {
      'nvim-lua/plenary.nvim'
    },
  }
  use {
    'nvim-telescope/telescope.nvim',
    requires = {
      {'nvim-lua/popup.nvim'},
      {'nvim-lua/plenary.nvim'},
      {'nvim-telescope/telescope-fzy-native.nvim'}
    }
  }
  use { 'nvim-treesitter/nvim-treesitter', run = ':TSUpdate' }
  use 'plasticboy/vim-markdown'
  use 'tpope/vim-abolish'
  use 'tpope/vim-commentary'
  use 'tpope/vim-surround'
  use 'tpope/vim-unimpaired'
end, {
  display = {
    open_fn = function()
      return util.float({ border = 'single' })
    end
  }
})
