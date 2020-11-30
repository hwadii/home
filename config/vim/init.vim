call plug#begin('~/.config/nvim/bundle/')
Plug 'chriskempson/base16-vim'
Plug 'junegunn/seoul256.vim'
Plug 'nanotech/jellybeans.vim'
Plug 'jnurmine/Zenburn'
Plug 'axvr/photon.vim'
Plug 'andreypopp/vim-colors-plain'
Plug 'justinmk/vim-dirvish'
Plug 'jiangmiao/auto-pairs'
Plug 'tpope/vim-commentary'
Plug 'tpope/vim-surround'
Plug 'airblade/vim-gitgutter'
Plug 'lambdalisue/gina.vim'
Plug 'tpope/vim-surround'
Plug 'tpope/vim-sleuth'
Plug 'tpope/vim-unimpaired'
Plug 'tpope/vim-abolish'
Plug 'AndrewRadev/splitjoin.vim'
Plug 'ledger/vim-ledger'
Plug 'neoclide/coc.nvim', {'branch': 'release'}
Plug 'nvim-treesitter/nvim-treesitter', { 'commit': 'e45ab15' }
Plug 'sheerun/vim-polyglot'
Plug 'plasticboy/vim-markdown'
Plug 'axvr/org.vim'
Plug 'airblade/vim-rooter'
" Plug 'junegunn/fzf', { 'dir': '~/.fzf', 'do': './install --all', 'branch': 'fc7630a66d8b07ec90603f7919f8aadf891783ac' }
" Plug 'junegunn/fzf', { 'dir': '~/.fzf', 'do': './install --all' }
" Plug 'junegunn/fzf.vim'
Plug 'nvim-lua/popup.nvim'
Plug 'nvim-lua/plenary.nvim'
Plug 'nvim-telescope/telescope.nvim'
Plug 'wincent/loupe'
call plug#end()

set encoding=utf-8
scriptencoding utf-8
set termguicolors
colorscheme plain

set t_Co=256
set background=dark
filetype plugin indent on
syntax on

" coc stuff
set hidden
set nobackup
set nowritebackup
set updatetime=300
set shortmess+=c
set signcolumn=yes
set showbreak=↳\ 
set breakindentopt=shift:2
set autoindent
set expandtab
set shiftwidth=2
set softtabstop=2
set ffs=unix,dos,mac
set autoread
set number
set rnu
set inccommand=nosplit
set backspace=start,eol,indent
set breakindent
set linebreak
set noswapfile
set mouse=a
set shellcmdflag=-ic
set nojoinspaces                      " don't autoinsert two spaces after '.', '?', '!' for join command
set ignorecase
set smartcase " make search case insensitive by default
set redrawtime=10000
set completeopt=menuone,noinsert,noselect
set splitbelow  " Splitting a window will put the new window below the current
set splitright  " Splitting a window will put the new window right of the current
set cursorline

let maplocalleader = "\<space>"
let mapleader = ","

let g:fzf_layout = { 'down': '~30%' }
" let g:fzf_options = '--preview "bat --style numbers,changes --color=always --decorations=always {} | head -500"'
let g:fzf_options = ''
let g:fzf_preview_window = ''

let g:gitgutter_sign_modified = '!!'
let g:gitgutter_sign_modified_removed = '!_'

autocmd TextYankPost * silent! lua require'vim.highlight'.on_yank {higroup="IncSearch", timeout=1000}
