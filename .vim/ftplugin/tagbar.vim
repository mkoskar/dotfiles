" vim: fdm=marker

if exists('b:did_ftplugin') | finish | endif
let s:cpo_save = &cpo
set cpo&vim

" ----------------------------------------

call utils#bufSpecial()

"nmap <buffer> <silent> go p
nnoremap <buffer> <silent> go :exec "normal \<lt>cr>"
    \ \| match IncSearch /\k*\%#\k*/ \| wincmd p<CR>

" ----------------------------------------

let &cpo = s:cpo_save
let b:did_ftplugin = 1
