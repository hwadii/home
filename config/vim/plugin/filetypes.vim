function! s:set_prose_options() abort
  setlocal spell
  setlocal spelllang=en
  setlocal textwidth=80
endfunction

au Filetype ruby set colorcolumn=140
au Filetype typescript,javascript set colorcolumn=120
au Filetype go setlocal noexpandtab tabstop=4 shiftwidth=4
au FileType text,tex,markdown,org call s:set_prose_options()
au FileType help set nolist
