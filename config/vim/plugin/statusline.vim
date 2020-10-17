hi StatusLine cterm=reverse ctermfg=95 ctermbg=187 gui=reverse guifg=#252525 guibg=#DFDEBD
hi StatusLineNC cterm=reverse ctermfg=235 ctermbg=187 gui=reverse guifg=#333233 guibg=#9e9e9e
hi User1 gui=bold guifg=#252525 guibg=#E19972
hi User2 gui=bold guifg=#E19972 guibg=#252525
hi User3 gui=italic guifg=#252525 guibg=#9e9e9e
hi User4 gui=italic guifg=#DFDEBD guibg=#252525
hi User5 gui=bold guifg=#DFDEBD guibg=#252525
hi User6 gui=bold guifg=#252525 guibg=#cccccc
hi User7 gui=bold guifg=#cccccc guibg=#252525

augroup SL
	autocmd!
	autocmd WinEnter,BufEnter * call hwadii#statusline#active()
	autocmd WinLeave * call hwadii#statusline#inactive()
augroup END
