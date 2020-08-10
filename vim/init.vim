call plug#begin('~/.config/nvim/bundle/')
" Theme
Plug 'itchyny/lightline.vim'
Plug 'chriskempson/base16-vim'
" QoL
Plug 'justinmk/vim-dirvish'
Plug 'jiangmiao/auto-pairs'
Plug 'tpope/vim-commentary'
Plug 'tpope/vim-surround'
Plug 'airblade/vim-gitgutter'
Plug 'tpope/vim-fugitive'
Plug 'tpope/vim-surround'
Plug 'tpope/vim-sleuth'
Plug 'AndrewRadev/splitjoin.vim'
" Modern web dev
Plug 'neoclide/coc.nvim', {'branch': 'release'}
Plug 'sheerun/vim-polyglot'
Plug 'plasticboy/vim-markdown'
" Fuzzy finder
Plug 'airblade/vim-rooter'
Plug 'junegunn/fzf', { 'dir': '~/.fzf', 'do': './install --all' }
Plug 'junegunn/fzf.vim'
Plug 'lervag/vimtex'

" wiki things
" Plug 'vimwiki/vimwiki'
" Plug 'fcpg/vim-waikiki'

call plug#end()
set termguicolors
colorscheme base16-circus
let g:lightline = {
      \ 'colorscheme': 'Tomorrow_Night',
      \ 'active': {
      \   'left': [ [ 'mode', 'paste' ],
      \             [ 'gitbranch', 'readonly', 'filename', 'modified' ] ]
      \ },
      \ 'component_function': {
      \   'gitbranch': 'FugitiveHead',
      \ },
      \ }

set t_Co=256
set background=dark
filetype plugin indent on
syntax on

" coc stuff
set hidden
set nobackup
set nowritebackup
set cmdheight=2
set updatetime=300
set shortmess+=c
set signcolumn=yes
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
set noshowmode
set mouse=a
set shellcmdflag=-ic
set nolist
set listchars+=space:·
set ignorecase
set smartcase " make search case insensitive by default

set splitbelow  " Splitting a window will put the new window below the current
set splitright  " Splitting a window will put the new window right of the current

let localleader = "<space>"
let g:vim_markdown_folding_disabled = 1
let g:vim_markdown_math = 1
let g:vim_markdown_frontmatter = 1
let g:vim_markdown_strikethrough = 1

vnoremap <silent><A-j> :m '>+1<CR>gv=gv
vnoremap <silent><A-k> :m '<-2<CR>gv=gv
nnoremap <silent><A-j> :m .+1<CR>==
nnoremap <silent><A-k> :m .-2<CR>==
inoremap <silent><A-j> <Esc>:m .+1<CR>==gi
inoremap <silent><A-k> <Esc>:m .-2<CR>==gi
inoremap <silent><C-d> <Del>
nnoremap <esc> :noh<return><esc>
map <C-h> <C-w>h
map <C-j> <C-w>j
map <C-k> <C-w>k
map <C-l> <C-w>l
nnoremap <silent><leader>p :Prettier<return>
nnoremap <silent><leader>li :set list!<CR>
nmap <leader>o <Plug>(coc-rename)
map <leader>e :e <C-R>=expand("%:p:h") . "/" <CR>

nnoremap <C-p> :Files<CR>
nnoremap <Leader>b :Buffers<CR>
nnoremap <Leader>h :History<CR>
nnoremap <Leader>r :Rg<CR>
nnoremap <Leader>l :Lines!<CR>

let g:netrw_liststyle = 3
let g:netrw_altv = 1
let g:netrw_banner = 0
" let g:vimwiki_list = [{'path': '~/things/vimwiki/'}]
let g:waikiki_roots = ['~/code/notes/']
let g:waikiki_default_maps = 1
let g:waikiki_done = "x"

function! s:check_back_space() abort
  let col = col('.') - 1
  return !col || getline('.')[col - 1]  =~ '\s'
endfunction

let g:user_emmet_install_global = 0
autocmd FileType html,css EmmetInstall
autocmd CursorHold * silent call CocActionAsync('highlight')

inoremap <silent><expr> <Tab>
      \ pumvisible() ? "\<C-n>" :
      \ <SID>check_back_space() ? "\<Tab>" :
      \ coc#refresh()

inoremap <silent><expr> <c-space> coc#refresh()
inoremap <expr> <cr> pumvisible() ? "\<C-y>" : "\<C-g>u\<CR>"
inoremap <expr> <Tab> pumvisible() ? "\<C-n>" : "\<Tab>"
inoremap <expr> <S-Tab> pumvisible() ? "\<C-p>" : "\<S-Tab>"
autocmd! CompleteDone * if pumvisible() == 0 | pclose | endif

nmap <silent> gd <Plug>(coc-definition)
nmap <silent> gy <Plug>(coc-type-definition)
nmap <silent> <leader>i <Plug>(coc-implementation)
nmap <silent> gr <Plug>(coc-references)
nmap <silent> <leader>ts <Plug>(coc-codeaction)
nnoremap <leader><leader> <c-^>
nnoremap <silent> K :call <SID>show_documentation()<CR>

nmap <leader>gs :Gstatus<cr>
nmap <leader>gc :Gcommit<cr>
nmap <leader>ga :Gwrite<cr>
nmap <leader>gl :Glog<cr>
nmap <leader>gd :Gdiff<cr>

function! s:show_documentation()
  if (index(['vim','help'], &filetype) >= 0)
    execute 'h '.expand('<cword>')
  else
    call CocAction('doHover')
  endif
endfunction

command! -nargs=0 Prettier :CocCommand prettier.formatFile
vmap <leader>f  <Plug>(coc-format-selected)
nmap <leader>f  <Plug>(coc-format-selected)
nmap <leader>ac  <Plug>(coc-codeaction)
nmap <leader>qf  <Plug>(coc-fix-current)
nmap <silent> [g <Plug>(coc-diagnostic-prev)
nmap <silent> ]g <Plug>(coc-diagnostic-next)

au Filetype ruby set colorcolumn=140
au Filetype typescript,javascript set colorcolumn=120
au Filetype go setlocal noexpandtab tabstop=4 shiftwidth=4
autocmd FileType text setlocal textwidth=80
autocmd FileType tex setlocal textwidth=80
autocmd FileType markdown setlocal textwidth=80

hi Todo gui=bold,italic cterm=bold,italic
hi Comment gui=italic cterm=italic

if exists('##TextYankPost')
  autocmd TextYankPost * silent! lua require'vim.highlight'.on_yank('IncSearch')
endif
let g:tex_flavor = 'latex'
