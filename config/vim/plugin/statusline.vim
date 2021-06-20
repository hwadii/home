augroup SL
	autocmd!
	autocmd WinEnter,BufEnter * lua require('wadii.statusline').active()
	autocmd WinLeave * lua require('wadii.statusline').inactive()
augroup END
