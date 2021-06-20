augroup SL
	autocmd!
	autocmd WinEnter,BufEnter * lua require('statusline').active()
	autocmd WinLeave * lua require('statusline').inactive()
augroup END
