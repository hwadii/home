local Path = require('plenary.path')

function active()
  vim.o.statusline = ''
  vim.o.statusline = vim.o.statusline .. '🌸 '
  vim.o.statusline = vim.o.statusline .. '%<'
  vim.o.statusline = vim.o.statusline .. '%p'
  vim.o.statusline = vim.o.statusline .. '%5*'
  vim.o.statusline = vim.o.statusline .. ''
  vim.o.statusline = vim.o.statusline .. '%*'
  vim.o.statusline = vim.o.statusline .. ' '
  vim.o.statusline = vim.o.statusline .. '%4*'
  vim.o.statusline = vim.o.statusline .. '%r'
  vim.o.statusline = vim.o.statusline .. 'signs'
  vim.o.statusline = vim.o.statusline .. '%*'
  -- setlocal statusline+=\ 
  -- setlocal statusline+=%<    " Truncation point
  -- setlocal statusline+=%{hwadii#statusline#fileprefix()} " Relative path
  -- setlocal statusline+=%5*
  -- setlocal statusline+=%t   " filename
  -- setlocal statusline+=%*
  -- setlocal statusline+=\ 
  -- setlocal statusline+=%4*
  -- setlocal statusline+=%m  " modified
  -- setlocal statusline+=%y   " filetype
  -- setlocal statusline+=\ 
  -- setlocal statusline+=%*
  -- setlocal statusline+=%=
  -- setlocal statusline+=%4*
  -- setlocal statusline+=%r   " readonly
  -- setlocal statusline+=%{get(b:,'gitsigns_status','')}
  -- setlocal statusline+=\ 
  -- setlocal statusline+=%{hwadii#statusline#branch()}
  -- setlocal statusline+=%*
  -- setlocal statusline+=\ 
  -- setlocal statusline+=%7*
  -- setlocal statusline+=%*
  -- setlocal statusline+=%6*
  -- setlocal statusline+=%{printf('\ ℓ\ %02d/%02d',line('.'),line('$'))} " line number
  -- setlocal statusline+=\ \ 
  -- setlocal statusline+=%{printf('c\ %02d/%02d\ ',col('.'),col('$'))} " col number
  -- setlocal statusline+=%*

end
