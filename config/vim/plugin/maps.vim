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
nnoremap <C-p> <cmd>Telescope find_files<CR>
nnoremap <Leader>pp <cmd>Telescope<cr>
nnoremap <Leader>s <cmd>Telescope live_grep<cr>
nnoremap <Leader><Leader>s <cmd>Telescope grep_string<cr>
nnoremap <localleader>b <cmd>Telescope buffers<cr>
nnoremap <Leader>h <cmd>Telescope oldfiles<cr>
nnoremap <Leader>c <cmd>Telescope git_commits<cr>
nnoremap <Leader>o <cmd>Telescope lsp_document_symbols<cr>
nnoremap <Leader>r <cmd>Telescope lsp_references<cr>
nnoremap <C-s> <cmd>Telescope current_buffer_fuzzy_find<cr>
nnoremap - <cmd>Telescope file_browser<cr>

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
nnoremap <silent> gR  <cmd>lua vim.lsp.buf.rename()<CR>
nnoremap <silent> <leader>f   <cmd>lua vim.lsp.buf.formatting()<CR>
nnoremap <silent> ga  <cmd>lua vim.lsp.buf.code_action()<CR>
nnoremap <silent> ]g <cmd>lua vim.lsp.diagnostic.goto_next()<cr>
nnoremap <silent> [g <cmd>lua vim.lsp.diagnostic.goto_prev()<cr>
inoremap <silent> <c-]> <cmd>lua vim.lsp.buf.signature_help()<CR>
nnoremap <silent> <c-]> <cmd>lua vim.lsp.buf.signature_help()<CR>

nnoremap <localleader>o  <cmd>lua vim.lsp.buf.document_symbol()<cr>
nnoremap <localleader>d  <cmd>lua vim.lsp.diagnostic.get_all()<cr>
nnoremap <localleader>i  <cmd>lua vim.lsp.diagnostic.show_line_diagnostics()<cr>

inoremap <expr> <Tab>   pumvisible() ? "\<C-n>" : "\<Tab>"
inoremap <expr> <S-Tab> pumvisible() ? "\<C-p>" : "\<S-Tab>"

inoremap <silent><expr> <C-Space> compe#complete()
inoremap <silent><expr> <CR>      compe#confirm('<CR>')
inoremap <silent><expr> <C-e>     compe#close('<C-e>')

inoremap <expr> <CR> pumvisible() ? "\<C-y>" : "\<C-g>u\<CR>"
inoremap <expr> <C-n> pumvisible() ? '<C-n>' :
  \ '<C-n><C-r>=pumvisible() ? "\<lt>Down>" : ""<CR>'
inoremap <expr> <M-,> pumvisible() ? '<C-n>' :
  \ '<C-x><C-o><C-n><C-p><C-r>=pumvisible() ? "\<lt>Down>" : ""<CR>'

" term
tnoremap <M-[> <C-\><C-n>

" vsnip
" imap <expr> <C-l>   vsnip#available(1)  ? '<Plug>(vsnip-expand-or-jump)' : '<C-l>'
" smap <expr> <C-l>   vsnip#available(1)  ? '<Plug>(vsnip-expand-or-jump)' : '<C-l>'
" imap <expr> <Tab>   vsnip#jumpable(1)   ? '<Plug>(vsnip-jump-next)'      : '<Tab>'
" smap <expr> <Tab>   vsnip#jumpable(1)   ? '<Plug>(vsnip-jump-next)'      : '<Tab>'
" imap <expr> <S-Tab> vsnip#jumpable(-1)  ? '<Plug>(vsnip-jump-prev)'      : '<S-Tab>'
" smap <expr> <S-Tab> vsnip#jumpable(-1)  ? '<Plug>(vsnip-jump-prev)'      : '<S-Tab>'
