au Filetype ruby set colorcolumn=140
au Filetype typescript,javascript set colorcolumn=120
au Filetype go setlocal noexpandtab tabstop=4 shiftwidth=4
au FileType text call set_prose_options()
au FileType tex call set_prose_options()
au FileType markdown call set_prose_options()
au FileType help set nolist

function! s:set_prose_options()
  setlocal spell
  setlocal spelllang=en
  setlocal textwidth=80
endfunction
