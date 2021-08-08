" general
nnoremap <silent> <A-j> :m .+1<CR>==
nnoremap <silent> <A-k> :m .-2<CR>==
vnoremap <silent> <A-j> :m '>+1<CR>gv=gv
vnoremap <silent> <A-k> :m '<-2<CR>gv=gv
inoremap <silent><C-d> <Del>
nnoremap <silent><esc> :noh<return><esc>
map <leader>e :e <C-R>=expand("%:p:h") . "/" <CR>
nnoremap <C-/> :set hlsearch!<cr>
nmap <A-[> :vertical resize -5<cr>
nmap <A-]> :vertical resize +5<cr>

" Gina
nmap <leader>gs :Gina status -s<cr>
nmap <leader>gc :Gina commit<cr>
nmap <leader>gp :Gina push<cr>
nmap <leader>gl :Gina log<cr>
nmap <leader>gd :Gina diff<cr>
nmap <leader>gb :Gina blame<cr>
nmap <leader>geb <Plug>(gina-blame-echo)
nmap <leader>gv :<C-U>echom gina#component#repo#branch()<CR>

" telescope
nnoremap <C-p> <cmd>Telescope find_files find_command=fd,--hidden,-E.git,-tf<CR>
nnoremap <Leader>pp <cmd>Telescope<cr>
nnoremap <Leader>s <cmd>Telescope live_grep<cr>
nnoremap <Leader><Leader>s <cmd>Telescope grep_string<cr>
nnoremap <localleader>b <cmd>Telescope buffers<cr>
nnoremap <Leader>h <cmd>Telescope oldfiles<cr>
nnoremap <Leader>c <cmd>Telescope git_commits<cr>
nnoremap <Leader>o <cmd>Telescope lsp_document_symbols<cr>
nnoremap <Leader>r <cmd>Telescope lsp_references<cr>
nnoremap <silent>ga <cmd>Telescope lsp_code_actions<cr>
nnoremap <C-s> <cmd>Telescope current_buffer_fuzzy_find<cr>

" tabs
nnoremap <silent> [t gT
nnoremap <silent> ]t gt

command Today :r!date -Idate

" term
tnoremap <M-[> <C-\><C-n>
