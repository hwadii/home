call plug#begin('~/.config/nvim/bundle/')
" Theme
Plug 'patstockwell/vim-monokai-tasty'
Plug 'itchyny/lightline.vim'
Plug 'morhetz/gruvbox'
Plug 'crusoexia/vim-monokai'
Plug 'chriskempson/base16-vim'
" QoL
Plug 'jiangmiao/auto-pairs'
Plug 'tpope/vim-commentary'
Plug 'airblade/vim-gitgutter'
Plug 'machakann/vim-highlightedyank'
Plug 'tpope/vim-fugitive'
" Modern web dev
Plug 'mattn/emmet-vim'
Plug 'neoclide/coc.nvim', {'branch': 'release'}
Plug 'sheerun/vim-polyglot'
Plug 'plasticboy/vim-markdown'
" Fuzzy finder
Plug 'airblade/vim-rooter'
Plug 'junegunn/fzf', { 'dir': '~/.fzf', 'do': './install --all' }
Plug 'junegunn/fzf.vim'

" wiki things
" Plug 'vimwiki/vimwiki'
Plug 'fcpg/vim-waikiki'

call plug#end()
set termguicolors
let g:monokai_term_italic = 1
colorscheme monokai
      " \ 'colorscheme': 'monokai_tasty',
let g:lightline = {
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
set shellcmdflag=-ic
set list
set listchars+=space:·
set ignorecase
set smartcase " make search case insensitive by default

set splitbelow  " Splitting a window will put the new window below the current
set splitright  " Splitting a window will put the new window right of the current

vnoremap <silent><A-j> :m '>+1<CR>gv=gv
vnoremap <silent><A-k> :m '<-2<CR>gv=gv
nnoremap <silent><A-j> :m .+1<CR>==
nnoremap <silent><A-k> :m .-2<CR>==
inoremap <silent><A-j> <Esc>:m .+1<CR>==gi
inoremap <silent><A-k> <Esc>:m .-2<CR>==gi
nnoremap <esc> :noh<return><esc>
map <C-h> <C-w>h
map <C-j> <C-w>j
map <C-k> <C-w>k
map <C-l> <C-w>l
nnoremap <silent><leader>p :Prettier<return>
nmap <leader>rn <Plug>(coc-rename)

nnoremap <C-p> :Files<CR>
nnoremap <Leader>o :Files!<CR>
nnoremap <Leader>b :Buffers<CR>
nnoremap <Leader>h :History<CR>
nnoremap <Leader>r :Rg<CR>

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
nmap <silent> gi <Plug>(coc-implementation)
nmap <silent> gr <Plug>(coc-references)
nmap <silent> <leader>ts <Plug>(coc-codeaction)
nnoremap <leader><leader> <c-^>
nnoremap <silent> K :call <SID>show_documentation()<CR>

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

au Filetype ruby set colorcolumn=140
au Filetype typescript,javascript set colorcolumn=120
au Filetype typescript set shiftwidth=4
au Filetype go setlocal noexpandtab tabstop=4 shiftwidth=4
au Filetype scss,css set shiftwidth=2

" whitespace errors
" if has('autocmd')
  " autocmd BufWinEnter <buffer> match Error /\s\+$/
  " autocmd InsertEnter <buffer> match Error /\s\+\%#\@<!$/
  " autocmd InsertLeave <buffer> match Error /\s\+$/
  " autocmd BufWinLeave <buffer> call clearmatches()
" endif

" italics
if (g:colors_name != 'monokai')
  hi Todo gui=bold,italic cterm=bold,italic
  hi Comment gui=italic cterm=italic
  hi jsClassKeyword gui=italic cterm=italic
  hi jsGlobalObjects gui=italic cterm=italic
  hi jsThis gui=italic cterm=italic
  hi jsSuper gui=italic cterm=italic
  hi jsFuncArgRest gui=italic cterm=italic
  hi jsFuncArgs gui=italic cterm=italic
  hi jsStorageClass gui=italic cterm=italic
  hi jsDocTags gui=italic cterm=italic
  hi jsFunction gui=italic cterm=italic
  " hi typescriptIdentifier gui=italic cterm=italic
  hi typescriptFuncType gui=italic cterm=italic
  hi typescriptArrowFuncArg gui=italic cterm=italic
  hi typescriptCall gui=italic cterm=italic
  hi typescriptClassKeyword gui=italic cterm=italic
  hi typescriptInterfaceKeyword gui=italic cterm=italic
  hi typescriptTypeReference gui=italic cterm=italic
  hi typescriptTypeParameter gui=italic cterm=italic
  hi cssURL gui=underline,italic cterm=underline,italic
endif
