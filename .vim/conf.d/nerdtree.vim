" vim: fdm=marker

packadd! nerdtree

let NERDTreeAutoDeleteBuffer = 1
let NERDTreeBookmarksFile = $VIMDIR . '/NERDTreeBookmarks'
let NERDTreeCaseSensitiveSort = 1
let NERDTreeHijackNetrw = 0
let NERDTreeIgnore = ['\.git$']
let NERDTreeMapCWD = 'cD'
let NERDTreeMapHelp = '<F1>'
let NERDTreeShowBookmarks = 1
let NERDTreeShowHidden = 1
let NERDTreeStatusline = "%{exists('b:NERDTree') ? fnamemodify(b:NERDTree.root.path.str(), ':p:~') : ''}"
let NERDTreeWinSize = 31

nnoremap <silent> <Leader>tt :NERDTreeToggle<CR>
nnoremap <silent> <Leader>to :NERDTreeFocus<CR>
nnoremap <silent> <Leader>tf :NERDTreeFind<CR>

augroup nerdtree_conf
    autocmd!
    autocmd FileType nerdtree
        \  call utils#bufSpecial()
        \| let b:ft_tabline = 1
        \| nmap <buffer> <silent> <nowait> s :call nerdtree#ui_glue#invokeKeyMap('s')<CR>
augroup END
