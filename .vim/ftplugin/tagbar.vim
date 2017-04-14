if exists('b:did_ftplugin')
    finish
endif

"nmap <buffer> <silent> go p
nnoremap <buffer> <silent> go :exec "normal \<lt>cr>"
    \ \| match IncSearch /\k*\%#\k*/ \| wincmd p<CR>
