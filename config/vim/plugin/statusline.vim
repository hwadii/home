hi StatusLine cterm=bold,reverse ctermfg=95 ctermbg=187 gui=bold,reverse guifg=#252525 guibg=#DFDEBD
hi StatusLineNC cterm=reverse ctermfg=235 ctermbg=187 gui=reverse guifg=#333233 guibg=#9e9e9e
hi User1 cterm=bold ctermfg=173 gui=bold guifg=#252525 guibg=#E19972
hi User2 cterm=bold ctermfg=173 gui=bold guifg=#E19972 guibg=#252525
hi User3 cterm=bold ctermfg=173  gui=italic guifg=#252525 guibg=#9e9e9e

function! statusline#branch() abort
  let branch = gina#component#repo#branch()
  if (branch != "")
    return "[".branch."] "
  else
    return branch
endfunction

augroup SL
	autocmd!
	autocmd WinEnter,BufEnter * call hwadii#statusline#active()
	autocmd WinLeave * call hwadii#statusline#inactive()
augroup END

