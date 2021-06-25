" vim: fdm=marker

packadd! bufexplorer

" TODO:
"
" * seems to mess up alternate buffer

let bufExplorerFindActive = 0
let bufExplorerShowNoName = 1
let bufExplorerShowRelativePath = 1

nnoremap <silent> <Leader>bb :BufExplorer<CR>

augroup bufexplorer_conf
    autocmd!
    autocmd FileType bufexplorer call utils#bufSpecial()
augroup END

hi link bufExplorerActBuf Directory
hi link bufExplorerAltBuf Type
hi link bufExplorerCurBuf Statement
hi link bufExplorerHelp String
hi link bufExplorerHidBuf Normal
hi link bufExplorerInactBuf Normal
hi link bufExplorerMapping Identifier
hi link bufExplorerTitle PreProc
