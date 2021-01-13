vim.cmd [[packadd packer.nvim]]

return require('packer').startup(function()
  use {'wbthomason/packer.nvim', opt = true}

  use 'AndrewRadev/splitjoin.vim'
  use 'airblade/vim-gitgutter'
  use 'airblade/vim-rooter'
  use 'andreypopp/vim-colors-plain'
  use 'jonathanfilip/vim-lucius'
  use 'junegunn/seoul256.vim'
  use 'axvr/org.vim'
  use 'cohama/lexima.vim'
  use 'justinmk/vim-dirvish'
  use 'lambdalisue/gina.vim'
  use 'ledger/vim-ledger'
  use 'neovim/nvim-lspconfig'
  use 'norcalli/snippets.nvim'
  use 'norcalli/nvim-colorizer.lua'
  use 'nvim-lua/completion-nvim'
  use {
    'nvim-telescope/telescope.nvim',
    requires = {{'nvim-lua/popup.nvim'}, {'nvim-lua/plenary.nvim'}}
  }
  use 'nvim-treesitter/nvim-treesitter'
  use 'plasticboy/vim-markdown'
  use 'tpope/vim-abolish'
  use 'tpope/vim-commentary'
  use 'tpope/vim-sleuth'
  use 'tpope/vim-surround'
  use 'tpope/vim-unimpaired'
  use 'wincent/loupe'
end)
