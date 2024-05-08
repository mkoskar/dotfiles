" vim: fdm=marker

packadd! bufexplorer

" TODO: breaks alternate buffer

let bufExplorerFindActive = 0
let bufExplorerShowNoName = 1
let bufExplorerShowRelativePath = 1

nnoremap <silent> <Leader>bb :BufExplorer<CR>

augroup bufexplorer_conf
    autocmd!
    autocmd FileType bufexplorer
        \  call utils#bufSpecial()
        \| let b:ft_tabline = 1
augroup END
