if exists('b:did_ftplugin')
    finish
endif
let b:did_ftplugin = 1

redir => s:lsout | silent filter /^\[Location List\]/ ls! % | redir END
let b:qf_loclist = !empty(s:lsout)
let b:qf_prefix = b:qf_loclist ? 'l' : 'c'

function! s:QfOldest(prefix) abort
    try | exec a:prefix . 'older 999' | catch | endtry
endfunction

function! s:QfNewest(prefix) abort
    try | exec a:prefix . 'newer 999' | catch | endtry
endfunction

function! s:QfOlder(prefix) abort
    try | exec a:prefix . 'older' | catch | endtry
endfunction

function! s:QfNewer(prefix) abort
    try | exec a:prefix . 'newer' | catch | endtry
endfunction

call utils#bufSpecial()
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
nnoremap <buffer> <silent> go :exec "normal \<lt>cr>"
    \ \| match IncSearch /\k*\%#\k*/ \| wincmd p<CR>
nnoremap <buffer> <silent> << :call <SID>QfOlder(b:qf_prefix)<CR>
nnoremap <buffer> <silent> >> :call <SID>QfNewer(b:qf_prefix)<CR>
nnoremap <buffer> <silent> <Leader>< :call <SID>QfOldest(b:qf_prefix)<CR>
nnoremap <buffer> <silent> <Leader>> :call <SID>QfNewest(b:qf_prefix)<CR>
