call plug#begin('~/.config/nvim/bundle/')
Plug 'junegunn/seoul256.vim'
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
Plug 'nvim-treesitter/nvim-treesitter'
Plug 'sheerun/vim-polyglot'
Plug 'plasticboy/vim-markdown'
Plug 'airblade/vim-rooter'
Plug 'junegunn/fzf', { 'dir': '~/.fzf', 'do': './install --all' }
Plug 'junegunn/fzf.vim'
Plug 'wincent/loupe'
call plug#end()

set encoding=utf-8
scriptencoding utf-8
set termguicolors
let g:seoul256_background = 233
colorscheme seoul256

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
set showbreak=↪\ 
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

let maplocalleader = "\<space>"

let g:fzf_layout = { 'down': '~40%' }
let g:fzf_options = '--preview "bat --style numbers,changes --color=always --decorations=always {} | head -500"'

let g:gitgutter_sign_modified = '!!'
let g:gitgutter_sign_modified_removed = '!_'

autocmd TextYankPost * silent! lua require'vim.highlight'.on_yank {higroup="IncSearch", timeout=1000}
