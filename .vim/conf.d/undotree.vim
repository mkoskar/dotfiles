" vim: fdm=marker

packadd! undotree

let undotree_SetFocusWhenToggle = 1
let undotree_ShortIndicators = 1
let undotree_SplitWidth = 31

nnoremap <silent> <Leader>ut :UndotreeToggle<CR>
nnoremap <silent> <Leader>uo :UndotreeShow<CR>

augroup undotree_conf
    autocmd!
    autocmd FileType undotree call utils#bufSpecial()
augroup END
