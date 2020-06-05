" vim: fdm=marker

packadd! undotree

let undotree_SplitWidth = 31

nnoremap <silent> <F4> :silent UndotreeToggle<CR>

augroup undotree_conf
    autocmd!
    autocmd FileType undotree call utils#bufSpecial()
augroup END
