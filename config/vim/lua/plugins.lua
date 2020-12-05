vim.cmd [[packadd packer.nvim]]

return require('packer').startup(function()
  use 'wbthomason/packer.nvim'

  use 'andreypopp/vim-colors-plain'
  use 'justinmk/vim-dirvish'
  use 'jiangmiao/auto-pairs'
  use 'tpope/vim-commentary'
  use 'airblade/vim-gitgutter'
  use 'lambdalisue/gina.vim'
  use 'tpope/vim-surround'
  use 'tpope/vim-sleuth'
  use 'tpope/vim-unimpaired'
  use 'tpope/vim-abolish'
  use 'AndrewRadev/splitjoin.vim'
  use 'ledger/vim-ledger'
  use {
    'neoclide/coc.nvim',
    branch = 'release'
  }
  use {
    'nvim-treesitter/nvim-treesitter',
    commit = 'e45ab15',
  }
  use 'sheerun/vim-polyglot'
  use 'plasticboy/vim-markdown'
  use 'axvr/org.vim'
  use 'airblade/vim-rooter'
  use 'nvim-lua/popup.nvim'
  use 'nvim-lua/plenary.nvim'
  use 'nvim-telescope/telescope.nvim'
  use 'wincent/loupe'
end)
