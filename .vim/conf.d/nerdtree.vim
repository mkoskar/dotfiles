" vim: fdm=marker

packadd! nerdtree

let NERDTreeAutoDeleteBuffer = 1
let NERDTreeBookmarksFile = $VIMDIR . '/.NERDTreeBookmarks'
let NERDTreeCaseSensitiveSort = 1
let NERDTreeHijackNetrw = 0
let NERDTreeIgnore = ['^\.svn$', '^\.git$', '\.swp$', '\~$']
let NERDTreeMapCWD = 'cD'
let NERDTreeMapHelp = '<F1>'
let NERDTreeShowBookmarks = 1
let NERDTreeShowHidden = 1
let NERDTreeWinSize = 31

nnoremap <silent> <F1> :NERDTreeFind<CR>
nnoremap <silent> <F2> :NERDTreeToggle<CR>
nnoremap <silent> <Leader>tf :NERDTreeFind<CR>
nnoremap <silent> <Leader>tt :NERDTreeToggle<CR>

augroup nerdtree_conf
    autocmd!
    autocmd FileType nerdtree call utils#bufSpecial()
augroup END
