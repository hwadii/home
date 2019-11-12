filetype plugin indent on
set nonumber
set rnu
set autoindent
set ruler
set expandtab
set shiftwidth=2
set softtabstop=2
syntax on

nnoremap <A-j> :m .+1<CR>==
nnoremap <A-k> :m .-2<CR>==
inoremap <A-j> <Esc>:m .+1<CR>==gi
inoremap <A-k> <Esc>:m .-2<CR>==gi

nnoremap <esc> :noh<return><esc>
