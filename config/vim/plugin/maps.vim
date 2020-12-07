" general
nnoremap <silent> <A-j> :m .+1<CR>==
nnoremap <silent> <A-k> :m .-2<CR>==
inoremap <silent> <A-j> <Esc>:m .+1<CR>==gi
inoremap <silent> <A-k> <Esc>:m .-2<CR>==gi
vnoremap <silent> <A-j> :m '>+1<CR>gv=gv
vnoremap <silent> <A-k> :m '<-2<CR>gv=gv
inoremap <silent><C-d> <Del>
inoremap <C-c> <esc>
nnoremap <silent><esc> :noh<return><esc>
map <C-h> <C-w>h
map <C-j> <C-w>j
map <C-k> <C-w>k
map <C-l> <C-w>l
map <leader>e :e <C-R>=expand("%:p:h") . "/" <CR>
nnoremap <leader><leader> <c-^>
nnoremap <silent> <C-/> :set hlsearch!<cr>

" Gina
nmap <leader>gs :Gina status -s<cr>
nmap <leader>gc :Gina commit<cr>
nmap <leader>gp :Gina push<cr>
nmap <leader>gl :Gina log<cr>
nmap <leader>gd :Gina diff<cr>
nmap <leader>gb :Gina blame<cr>
nmap <leader>geb <Plug>(gina-blame-echo)
nmap <leader>gv :<C-U>echom gina#component#repo#branch()<CR>

" gitgutter
nnoremap <silent> <leader>hl :GitGutterLineHighlightsToggle<CR>

nnoremap <silent><leader>p :Prettier<return>

" telescope
nnoremap <C-p> :lua require('telescope.builtin').find_files({previewer = false})<CR>
nnoremap <Leader>r <cmd>Telescope live_grep<cr>
nnoremap <Leader>b <cmd>Telescope buffers<cr>
nnoremap <Leader>h <cmd>Telescope oldfiles<cr>
nnoremap <Leader>c <cmd>Telescope git_commits<cr>
nnoremap <C-s> <cmd>Telescope current_buffer_fuzzy_find<cr>

" Goyo
nnoremap <silent> <localleader>gg  :Goyo<CR>

" Ledger
nnoremap <silent> glf :<C-U>%LedgerAlign<CR>

" tabs
nnoremap <silent> [t gT
nnoremap <silent> ]t gt

command Today :r!date -Idate

" nvim-lsp
nnoremap <silent> gd          <cmd>lua vim.lsp.buf.definition()<CR>
nnoremap <silent> K           <cmd>lua vim.lsp.buf.hover()<CR>
nnoremap <silent> <leader>i   <cmd>lua vim.lsp.buf.implementation()<CR>
nnoremap <silent> gy          <cmd>lua vim.lsp.buf.type_definition()<CR>
nnoremap <silent> gr          <cmd>lua vim.lsp.buf.references()<CR>
nnoremap <silent> gws         <cmd>lua vim.lsp.buf.workspace_symbol()<CR>
nnoremap <silent> <leader>rn  <cmd>lua vim.lsp.buf.rename()<CR>
nnoremap <silent> <leader>f   <cmd>lua vim.lsp.buf.formatting()<CR>
nnoremap <silent> <leader>ca  <cmd>lua vim.lsp.buf.code_action()<CR>
nnoremap <silent> ]g <cmd>lua vim.lsp.diagnostic.goto_next()<cr>
nnoremap <silent> [g <cmd>lua vim.lsp.diagnostic.goto_prev()<cr>

nnoremap <localleader>o  <cmd>lua vim.lsp.buf.document_symbol()<cr>
nnoremap <localleader>d  <cmd>lua vim.lsp.diagnostic.get_all()<cr>
nnoremap <localleader>i  <cmd>lua vim.lsp.diagnostic.show_line_diagnostics()<cr>
nnoremap <localleader>s  <cmd>lua vim.lsp.buf.signature_help()<CR>

inoremap <expr> <Tab>   pumvisible() ? "\<C-n>" : "\<Tab>"
inoremap <expr> <S-Tab> pumvisible() ? "\<C-p>" : "\<S-Tab>"
imap <silent> <c-space> <Plug>(completion_trigger)
imap <tab> <Plug>(completion_smart_tab)
imap <s-tab> <Plug>(completion_smart_s_tab)

inoremap <expr> <CR> pumvisible() ? "\<C-y>" : "\<C-g>u\<CR>"
inoremap <expr> <C-n> pumvisible() ? '<C-n>' :
  \ '<C-n><C-r>=pumvisible() ? "\<lt>Down>" : ""<CR>'
inoremap <expr> <M-,> pumvisible() ? '<C-n>' :
  \ '<C-x><C-o><C-n><C-p><C-r>=pumvisible() ? "\<lt>Down>" : ""<CR>'
