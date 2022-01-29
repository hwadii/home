augroup SL
	autocmd!
	autocmd WinEnter,BufEnter * call v:lua.wadii.statusline.active()
	autocmd WinLeave,BufLeave * call v:lua.wadii.statusline.inactive()
augroup END
