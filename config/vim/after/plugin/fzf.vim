inoremap <expr> <c-x><c-f> fzf#vim#complete#path('fd')
command! -bang -nargs=? -complete=dir Files
  \ call fzf#run(fzf#wrap(
  \   {
  \     'source': 'fd -E".git" --hidden --type f .\* '.(empty(<q-args>) ? '' : shellescape(<q-args>)),
  \     'options': g:fzf_options
  \   }, <bang>0))

