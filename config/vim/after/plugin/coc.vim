let g:coc_global_extensions = [
            \ 'coc-json',
            \ 'coc-python',
            \ 'coc-tsserver',
            \ 'coc-prettier',
            \ 'coc-html',
            \ 'coc-css',
            \ 'coc-angular',
            \ 'coc-solargraph',
            \ 'coc-rls',
            \ ]

command! -nargs=0 Prettier :CocCommand prettier.formatFile
command! -nargs=0 Format :call CocAction('format')
command! -nargs=0 OR   :call CocAction('runCommand', 'editor.action.organizeImport')

