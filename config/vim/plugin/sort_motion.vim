let g:sort_motion_flags = ""
function! s:sort_motion(mode) abort
	if (a:mode == 'line')
		execute "'[,']sort " . g:sort_motion_flags
	elseif (a:mode == 'V')
		execute "'<,'>sort " . g:sort_motion_flags
		echo a:mode
	endif
endfunction

nnoremap <silent> <Plug>SortMotion :<C-U>set opfunc=<SID>sort_motion<CR>g@
xnoremap <silent> <Plug>SortVisual :<C-U>call <SID>sort_motion(visualmode())<CR>
nmap <silent> gss <Plug>SortMotion
vmap <silent> gs <Plug>SortVisual
