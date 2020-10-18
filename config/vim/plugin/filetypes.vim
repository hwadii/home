au Filetype ruby set colorcolumn=140
au Filetype typescript,javascript set colorcolumn=120
au Filetype go setlocal noexpandtab tabstop=4 shiftwidth=4
au FileType text call SetProseOptions()
au FileType tex call SetProseOptions()
au FileType markdown call SetProseOptions()
au FileType help set nolist

function SetProseOptions()
  setlocal spell
  setlocal spelllang=en
  setlocal textwidth=80
endfunction
