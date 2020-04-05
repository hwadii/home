call plug#begin('~/.config/nvim/bundle/')
" Theme
Plug 'itchyny/lightline.vim'
Plug 'morhetz/gruvbox' 
Plug 'patstockwell/vim-monokai-tasty'
Plug 'crusoexia/vim-monokai'
" QoL
Plug 'jiangmiao/auto-pairs'
Plug 'tpope/vim-commentary'
Plug 'airblade/vim-gitgutter'
Plug 'machakann/vim-highlightedyank'
Plug 'tpope/vim-fugitive'
Plug 'sheerun/vim-polyglot'
" Modern web dev
Plug 'mattn/emmet-vim'
" Plug 'w0rp/ale'
Plug 'neoclide/coc.nvim', {'branch': 'release'}
" Fuzzy finder
Plug 'airblade/vim-rooter'
Plug 'junegunn/fzf', { 'dir': '~/.fzf', 'do': './install --all' }
Plug 'junegunn/fzf.vim'

" Plug 'Yggdroot/indentLine'
"   let g:indentLine_color_term = 200
"   let g:indentLine_char='│'
"   let g:indentLine_fileTypeExclude=['help', 'markdown', 'fzf', 'git']

" If used inside Tmux, this shows the vim airline statusline in Tmux's
" statusline
Plug 'edkolev/tmuxline.vim'

call plug#end()
set termguicolors
" let g:onehalf_terminal_italics=1
" let g:one_allow_italics = 1
" let g:gruvbox_italic=1
" let g:gruvbox_invert_selection=0
" let g:vim_monokai_tasty_italic = 1
let g:monokai_term_italic = 1
colorscheme monokai
let g:lightline = {
      \ 'colorscheme': 'monokai_tasty',
      \ 'active': {
      \   'left': [ [ 'mode', 'paste' ],
      \             [ 'gitbranch', 'readonly', 'filename', 'modified' ] ]
      \ },
      \ 'component_function': {
      \   'gitbranch': 'FugitiveHead'
      \ },
      \ }
" let g:lightline = {
"       \ 'colorscheme': 'monokai_tasty'
" \ }
" let g:lightline = {
"       \ 'colorscheme': 'gruvbox'
" \ }
set t_Co=256
set background=dark
filetype plugin indent on

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
set list listchars=tab:>-,space:·

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
" imap jj <Esc>
nnoremap <silent><leader>p :Prettier<return>
nnoremap <leader>sc :ALEToggle<CR>
nmap <leader>rn <Plug>(coc-rename)

nnoremap <C-p> :Files<CR>
nnoremap <Leader>o :Files!<CR>
nnoremap <Leader>b :Buffers<CR>
" nnoremap <Leader>h :History<CR>
nnoremap <Leader>r :Rg<CR>

let g:prettier#config#single_quote = 'true'
let g:prettier#config#jsx_bracket_same_line = 'false'
let g:prettier#config#arrow_parens = 'avoid'
let g:prettier#config#trailing_comma = 'es5'
let g:prettier#config#bracket_spacing = 'true'

let g:netrw_liststyle = 3
let g:netrw_altv = 1
let g:netrw_banner = 0

let g:tmuxline_powerline_separators = 0
let g:tmuxline_preset = {
      \'a'    : '#S',
      \'b'    : '#W',
      \'c'    : '',
      \'win'  : '#I #W',
      \'cwin' : '#I #W',
      \'x'    : '%a',
      \'y'    : '#W %R',
      \'z'    : ''}

function! s:check_back_space() abort
  let col = col('.') - 1
  return !col || getline('.')[col - 1]  =~ '\s'
endfunction

let g:user_emmet_install_global = 0
autocmd FileType html,css EmmetInstall

inoremap <silent><expr> <Tab>
      \ pumvisible() ? "\<C-n>" :
      \ <SID>check_back_space() ? "\<Tab>" :
      \ coc#refresh()

inoremap <silent><expr> <c-space> coc#refresh()
inoremap <expr> <cr> pumvisible() ? "\<C-y>" : "\<C-g>u\<CR>"

nmap <silent> gd <Plug>(coc-definition)
nmap <silent> gy <Plug>(coc-type-definition)
nmap <silent> gi <Plug>(coc-implementation)
nmap <silent> gr <Plug>(coc-references)
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
