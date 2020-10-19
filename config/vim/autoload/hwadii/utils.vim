scriptencoding utf-8

function! hwadii#utils#lorem() abort
  return 'hello'
endfunction

function! hwadii#utils#buf_only() abort
  execute '%bd | e#'
endfunction
