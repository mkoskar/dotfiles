" vim: fdm=marker

packadd! nerdtree

let NERDTreeAutoDeleteBuffer = 1
let NERDTreeBookmarksFile = $VIMDIR . '/NERDTreeBookmarks'
let NERDTreeCaseSensitiveSort = 1
let NERDTreeHijackNetrw = 0
let NERDTreeIgnore = []
let NERDTreeMapCWD = 'cD'
let NERDTreeMapHelp = '<F1>'
let NERDTreeShowBookmarks = 1
let NERDTreeShowHidden = 1
let NERDTreeWinSize = 31

nnoremap <silent> <Leader>tt :NERDTreeToggle<CR>
nnoremap <silent> <Leader>to :NERDTreeFocus<CR>
nnoremap <silent> <Leader>tf :NERDTreeFind<CR>

augroup nerdtree_conf
    autocmd!
    autocmd FileType nerdtree
        \  call utils#bufSpecial()
        \| nmap <buffer> <silent> <nowait> s :call nerdtree#ui_glue#invokeKeyMap('s')<CR>
augroup END
