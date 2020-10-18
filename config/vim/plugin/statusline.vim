augroup SL
	autocmd!
	autocmd WinEnter,BufEnter * call hwadii#statusline#active()
	autocmd WinLeave * call hwadii#statusline#inactive()
augroup END
