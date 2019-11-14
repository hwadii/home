" execute pathogen#infect()
call plug#begin('~/.config/nvim/bundle/')

Plug 'jiangmiao/auto-pairs'
Plug 'tpope/vim-commentary'
Plug 'airblade/vim-gitgutter'
" Plug 'neoclide/coc.nvim', {'branch': 'release'}
Plug 'sheerun/vim-polyglot'
Plug 'prettier/vim-prettier', { 'do': 'npm install' }
Plug 'mattn/emmet-vim'

call plug#end()

set termguicolors
colorscheme monokai_pro

set nonumber
set rnu
set autoindent
set ruler
set expandtab
set shiftwidth=2
set softtabstop=2

nnoremap <A-j> :m .+1<CR>==
nnoremap <A-k> :m .-2<CR>==
inoremap <A-j> <Esc>:m .+1<CR>==gi
inoremap <A-k> <Esc>:m .-2<CR>==gi
nnoremap <esc> :noh<return><esc>

let g:ale_completion_enabled = 1
set omnifunc=ale#completion#OmniFunc

nnoremap <leader><leader> <c-^>
