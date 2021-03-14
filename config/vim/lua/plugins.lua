vim.cmd [[packadd packer.nvim]]

return require('packer').startup(function()
  use {'wbthomason/packer.nvim', opt = true}

  use 'AndrewRadev/splitjoin.vim'
  use 'airblade/vim-rooter'
  use 'junegunn/seoul256.vim'
  use 'axvr/org.vim'
  use 'justinmk/vim-dirvish'
  use 'jiangmiao/auto-pairs'
  use 'lambdalisue/gina.vim'
  use 'ledger/vim-ledger'
  use 'neovim/nvim-lspconfig'
  use 'norcalli/snippets.nvim'
  use 'norcalli/nvim-colorizer.lua'
  use 'hrsh7th/nvim-compe'
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
  use 'nvim-treesitter/nvim-treesitter'
  use 'tpope/vim-abolish'
  use 'tpope/vim-commentary'
  use 'tpope/vim-sleuth'
  use 'tpope/vim-surround'
  use 'tpope/vim-unimpaired'
end)
