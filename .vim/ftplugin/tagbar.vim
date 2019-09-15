if exists('b:did_ftplugin')
    finish
endif
let b:did_ftplugin = 1

call utils#bufSpecial()

"nmap <buffer> <silent> go p
nnoremap <buffer> <silent> go :exec "normal \<lt>cr>"
    \ \| match IncSearch /\k*\%#\k*/ \| wincmd p<CR>
