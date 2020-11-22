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
nnoremap <leader>fp gqap
nnoremap <silent> <C-/> :set hlsearch!<cr>

" coc.vim
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
nnoremap <silent> K :call <SID>show_documentation()<CR>

nmap <leader>o <Plug>(coc-rename)

function s:show_documentation()
  if (index(['vim','help'], &filetype) >= 0)
    execute 'h '.expand('<cword>')
  else
    call CocAction('doHover')
  endif
endfunction

function s:check_back_space()
  let col = col('.') - 1
  return !col || getline('.')[col - 1]  =~ '\s'
endfunction

inoremap <silent><expr> <Tab>
      \ pumvisible() ? "\<C-n>" :
      \ <SID>check_back_space() ? "\<Tab>" :
      \ coc#refresh()

vmap <leader>f  <Plug>(coc-format-selected)
nmap <leader>f  <Plug>(coc-format-selected)
nmap <leader>ac  <Plug>(coc-codeaction)
nmap <leader>qf  <Plug>(coc-fix-current)
nmap <silent> [g <Plug>(coc-diagnostic-prev)
nmap <silent> ]g <Plug>(coc-diagnostic-next)

nnoremap <silent><nowait> <localleader>o  :<C-u>CocList outline<cr>
nnoremap <silent><nowait> <localleader>a  :<C-u>CocList diagnostics<cr>
nnoremap <silent><nowait> <localleader>s  :<C-u>CocList -I symbols<cr>
nnoremap <silent><nowait> <localleader>j  :<C-u>CocNext<CR>
nnoremap <silent><nowait> <localleader>k  :<C-u>CocPrev<CR>
nnoremap <silent><nowait> <localleader>p  :<C-u>CocListResume<CR>
nmap <silent> <localleader>i <Plug>(coc-diagnostic-info)
nnoremap <nowait><expr> <C-f> coc#float#has_scroll() ? coc#float#scroll(1) : "\<C-f>"
nnoremap <nowait><expr> <C-b> coc#float#has_scroll() ? coc#float#scroll(0) : "\<C-b>"
inoremap <nowait><expr> <C-f> coc#float#has_scroll() ? "\<c-r>=coc#float#scroll(1)\<cr>" : "\<Right>"
inoremap <nowait><expr> <C-b> coc#float#has_scroll() ? "\<c-r>=coc#float#scroll(0)\<cr>" : "\<Left>"

" Gina
nmap <leader>gs :Gina status -s<cr>
nmap <leader>gc :Gina commit<cr>
nmap <leader>gp :Gina push<cr>
nmap <leader>gl :Gina log<cr>
nmap <leader>gd :Gina diff<cr>
nmap <leader>gb :Gina blame<cr>
nmap <leader>geb <Plug>(gina-blame-echo)
nmap <leader>ger :<C-U>echom gina#component#repo#branch()<CR>

" gitgutter
nnoremap <silent> <leader>hl :GitGutterLineHighlightsToggle<CR>

" fzf
nnoremap <silent><leader>p :Prettier<return>
nnoremap <C-p> :Files<CR>
nnoremap <Leader>b :Buffers<CR>
nnoremap <Leader>h :History!<CR>
nnoremap <Leader>r :Find<CR>
nnoremap <Leader>/ :execute 'RG <c-r><c-w>'<CR>
nnoremap <Leader>l :Lines!<CR>
nnoremap <C-s> :<C-u>BLines<CR>
nnoremap <Leader>c :Commits<CR>

" Goyo
nnoremap <silent> <localleader>gg  :Goyo<CR>

" Ledger
nnoremap <silent> glf :<C-U>%LedgerAlign<CR>

" tabs
nnoremap <silent> [t gT
nnoremap <silent> ]t gt

command Today :r!date -Idate
