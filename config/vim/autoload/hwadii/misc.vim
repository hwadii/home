scriptencoding utf-8

function! hwadii#misc#check_back_space() abort
  let col = col('.') - 1
  return !col || getline('.')[col - 1]  =~ '\s'
endfunction

function! hwadii#misc#show_documentation() abort
  if (index(['vim','help'], &filetype) >= 0)
    execute 'h '.expand('<cword>')
  else
    call CocAction('doHover')
  endif
endfunction

function! hwadii#misc#set_prose_options() abort
  setlocal spell
  setlocal spelllang=en
  setlocal textwidth=80
endfunction
