scriptencoding utf-8

function! hwadii#statusline#gutterpadding() abort
  let l:signcolumn=0
  if exists('+signcolumn')
    if &signcolumn == 'yes'
      let l:signcolumn=2
    elseif &signcolumn == 'auto'
      if exists('*execute')
        let l:signs=execute('sign place buffer=' .bufnr('$'))
      else
        let l:signs=''
        silent! redir => l:signs
        silent execute 'sign place buffer=' . bufnr('$')
        redir END
      end
      if match(l:signs, 'line=') != -1
        let l:signcolumn=2
      endif
    endif
  endif

  let l:minwidth=2
  let l:gutterWidth=max([strlen(line('$')) + 1, &numberwidth, l:minwidth]) + l:signcolumn
  let l:padding=repeat(' ', l:gutterWidth - 1)
  return l:padding
endfunction

function! hwadii#statusline#lhs() abort
  let l:line=hwadii#statusline#gutterpadding()
  let l:line.=&modified ? '✘ ' : '  '
  return l:line
endfunction

function! hwadii#statusline#branch() abort
  let branch = gina#component#repo#branch()
  if (branch != "")
    return branch."⎇ "
  else
    return branch
endfunction

function! hwadii#statusline#fileprefix() abort
  let l:basename=expand('%:h')
  if l:basename ==# '' || l:basename ==# '.'
    return ''
  elseif has('modify_fname')
    return substitute(fnamemodify(l:basename, ':~:.'), '/$', '', '') . '/'
  else
    return substitute(l:basename . '/', '\C^' . $HOME, '~', '')
  endif
endfunction

function! hwadii#statusline#active() abort
  setlocal statusline=
  setlocal statusline+=%1*   "italic
  setlocal statusline+=%{hwadii#statusline#lhs()}
  setlocal statusline+=%*   " Reset highlight group.
  setlocal statusline+=%2*
  setlocal statusline+=
  setlocal statusline+=%*
  setlocal statusline+=\ 
  setlocal statusline+=%<   " Truncation point
  setlocal statusline+=%{hwadii#statusline#fileprefix()} " Relative path
  setlocal statusline+=%5*
  setlocal statusline+=%t   " filename
  setlocal statusline+=%*
  setlocal statusline+=\ 
  setlocal statusline+=%4*
  setlocal statusline+=%y   " filetype
  setlocal statusline+=\ 
  setlocal statusline+=%*
  setlocal statusline+=%=
  setlocal statusline+=%4*
  setlocal statusline+=%r   " readonly
  setlocal statusline+=%{hwadii#statusline#branch()}
  setlocal statusline+=%*
  setlocal statusline+=\ 
  setlocal statusline+=%7*
  setlocal statusline+=
  setlocal statusline+=%*
  setlocal statusline+=%6*
  setlocal statusline+=%{printf('\ ℓ\ %02d/%02d',line('.'),line('$'))} " line number
  setlocal statusline+=\ \ 
  setlocal statusline+=%{printf('c\ %02d/%02d\ ',col('.'),col('$'))} " col number
  setlocal statusline+=%*
endfunction

function! hwadii#statusline#inactive() abort
  setlocal statusline=
  setlocal statusline+=%{hwadii#statusline#gutterpadding()}
  setlocal statusline+=%f
endfunction