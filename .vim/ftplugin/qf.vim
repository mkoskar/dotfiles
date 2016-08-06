if exists('b:did_ftplugin') || exists('b:locl')
    finish
endif

let b:locl = 1
try | lopen | catch /:E776:/ | let b:locl = 0 | endtry
let b:prefix = b:locl ? 'l' : 'c'

function! QfOldest(prefix) abort
    try | exec a:prefix.'older 999' | catch | endtry
endfunction

function! QfNewest(prefix) abort
    try | exec a:prefix.'newer 999' | catch | endtry
endfunction

function! QfOlder(prefix) abort
    try | exec a:prefix.'older' | catch | endtry
endfunction

function! QfNewer(prefix) abort
    try | exec a:prefix.'newer' | catch | endtry
endfunction

call utils#BufSpecial()
setl nu nobl
wincmd J

noremap <buffer> - <Nop>
noremap <buffer> <F1> <Nop>
noremap <buffer> <F2> <Nop>
noremap <buffer> <F4> <Nop>
noremap <buffer> <F8> <Nop>
noremap <buffer> <Leader>tf <Nop>
noremap <buffer> <Leader>tt <Nop>
noremap <buffer> <Leader>bb <Nop>
noremap <buffer> <Leader>gg <Nop>
noremap <buffer> <Leader>G <Nop>

nmap <buffer> <silent> o <CR>
nnoremap <buffer> <silent> q :close<CR>
exec 'nnoremap <buffer> <silent> go :exec "normal \<lt>cr>" \|
          \ match IncSearch /\k*\%#\k*/ \| wincmd p<CR>'
nnoremap <buffer> <silent> << :call QfOlder(b:prefix)<CR>
nnoremap <buffer> <silent> >> :call QfNewer(b:prefix)<CR>
nnoremap <buffer> <silent> <Leader>< :call QfOldest(b:prefix)<CR>
nnoremap <buffer> <silent> <Leader>> :call QfNewest(b:prefix)<CR>
