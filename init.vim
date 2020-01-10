" execute pathogen#infect()
call plug#begin('~/.config/nvim/bundle/')
" Theme
" Plug 'morhetz/gruvbox'
" Plug 'nightsense/rusticated'
Plug 'crusoexia/vim-monokai'
Plug 'itchyny/lightline.vim'
" QoL
Plug 'jiangmiao/auto-pairs'
Plug 'tpope/vim-commentary'
Plug 'airblade/vim-gitgutter'
Plug 'machakann/vim-highlightedyank'
" Plug 'tpope/vim-unimpaired'
Plug 'sheerun/vim-polyglot'
" Modern web dev
Plug 'w0rp/ale'
Plug 'neoclide/coc.nvim', {'branch': 'release'}
" Plug 'mattn/emmet-vim'
" Fuzzy finder
Plug 'airblade/vim-rooter'
Plug 'junegunn/fzf', { 'dir': '~/.fzf', 'do': './install --all' }
Plug 'junegunn/fzf.vim'

" If used inside Tmux, this shows the vim airline statusline in Tmux's
" statusline
Plug 'edkolev/tmuxline.vim'

call plug#end()
set termguicolors
colorscheme monokai
set t_Co=256

" coc stuff
set hidden
set nobackup
set nowritebackup
set cmdheight=2
set updatetime=300
set shortmess+=c
set signcolumn=yes

set ffs=unix,dos,mac
set autoread
set number
set rnu
set autoindent
set expandtab
set shiftwidth=2
set softtabstop=2
set inccommand=nosplit
set backspace=start,eol,indent
set breakindent
set linebreak
set noswapfile
set cursorline  " highlight current line
set noshowmode
set mouse=a

set splitbelow  " Splitting a window will put the new window below the current
set splitright  " Splitting a window will put the new window right of the current

nnoremap <silent><A-j> :m .+1<CR>==
nnoremap <silent><A-k> :m .-2<CR>==
inoremap <silent><A-j> <Esc>:m .+1<CR>==gi
inoremap <silent><A-k> <Esc>:m .-2<CR>==gi
nnoremap <esc> :noh<return><esc>
map <C-h> <C-w>h
map <C-j> <C-w>j
map <C-k> <C-w>k
map <C-l> <C-w>l
map H ^
map L $
imap jj <Esc>
nnoremap <silent><leader>p :Prettier<return>
nnoremap <leader>sc :ALEToggle<CR>

nnoremap <C-p> :Files<CR>
nnoremap <Leader>b :Buffers<CR>
nnoremap <Leader>h :History<CR>
nnoremap <Leader>r :Rg<CR>

" let g:ale_completion_enabled = 1
let g:ale_linters_explicit = 1
let g:ale_fixers = {
\   'javascript': ['prettier'],
\   'css': ['prettier'],
\}
" let g:ale_fix_on_save = 1
" set omnifunc=ale#completion#OmniFunc
let g:prettier#config#single_quote = 'false'
let g:prettier#config#jsx_bracket_same_line = 'false'
let g:prettier#config#arrow_parens = 'avoid'
let g:prettier#config#trailing_comma = 'none'
let g:prettier#config#bracket_spacing = 'true'

let g:netrw_liststyle = 3
let g:netrw_altv = 1
let g:netrw_banner = 0

function! s:check_back_space() abort
  let col = col('.') - 1
  return !col || getline('.')[col - 1]  =~ '\s'
endfunction

inoremap <silent><expr> <Tab>
      \ pumvisible() ? "\<C-n>" :
      \ <SID>check_back_space() ? "\<Tab>" :
      \ coc#refresh()

inoremap <silent><expr> <c-space> coc#refresh()
inoremap <expr> <cr> pumvisible() ? "\<C-y>" : "\<C-g>u\<CR>"

" nmap <silent> gd <Plug>(coc-definition)
nmap <silent> gy <Plug>(coc-type-definition)
nmap <silent> gi <Plug>(coc-implementation)
nmap <silent> gr <Plug>(coc-references)
nnoremap <leader><leader> <c-^>

command! -nargs=0 Prettier :CocCommand prettier.formatFile
