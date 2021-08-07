" vim: fdm=marker

if exists('b:did_ftplugin') | finish | endif
let s:cpo_save = &cpo
set cpo&vim

" ----------------------------------------

"redir => s:lsout | silent filter /^\[Location List\]/ ls! % | redir END
"let b:qf_loclist = !empty(s:lsout)
let b:qf_loclist = get(getwininfo(win_getid())[0], 'loclist', 0)
let b:qf_prefix = b:qf_loclist ? 'l' : 'c'

let b:GetList = b:qf_loclist ? function('getloclist', [0]) : function('getqflist')
let b:SetList = b:qf_loclist ? function('setloclist', [0]) : function('setqflist')

let s:sid = expand('<SID>')
let &l:stl = "[%{b:qf_loclist ? 'LocList' : 'QfList'}"
let &l:stl .= '%( %{%' . s:sid . 'Nr()%}]%)'
let &l:stl .= '%( (%{%' . s:sid . 'Idx()%})%)'
let &l:stl .= '%( %<%{%' . s:sid . 'Title()%}%)'
let &l:stl .= '%=' . stlTail

function! s:Nr() abort
    return printf('%d/%d', b:GetList(#{nr: 0}).nr, b:GetList(#{nr: '$'}).nr)
endfunction

function! s:Idx() abort
    return printf('%d/%d', b:GetList(#{idx: 0}).idx, b:GetList(#{size: 0}).size)
endfunction

function! s:Title() abort
    return b:GetList(#{title: 0}).title
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

nmap <buffer> <silent> o <CR>
nnoremap <buffer> <silent> q :close<CR>
nnoremap <buffer> <silent> go
    \ :exec "normal \<lt>CR>" \| match IncSearch /\k*\%#\k*/ \| wincmd p<CR>

function! s:QfPrefixCmd(cmd) abort
    try | exec b:qf_prefix . a:cmd | catch | endtry
endfunction

command! -buffer -bar QfOlder call s:QfPrefixCmd('older')
command! -buffer -bar QfNewer call s:QfPrefixCmd('newer')
command! -buffer -bar QfOldest call s:QfPrefixCmd('older 999')
command! -buffer -bar QfNewest call s:QfPrefixCmd('newer 999')

nnoremap <buffer> <silent> << :QfOlder<CR>
nnoremap <buffer> <silent> >> :QfNewer<CR>
nnoremap <buffer> <silent> <Leader>< :QfOldest<CR>
nnoremap <buffer> <silent> <Leader>> :QfNewest<CR>

command! -buffer -bar QfClear call b:SetList([], 'r', #{title: '', items: []})
command! -buffer -bar QfFree call b:SetList([], 'f')
command! -buffer -bar QfNew call b:SetList([], ' ', #{title: ''})

function! s:QfReload() abort
    let ctx = b:GetList(#{context: 0}).context
    if empty(ctx) || !has_key(ctx, 'reload_func')
        call utils#echoError("Can't be reloaded")
        return
    endif
    if b:qf_loclist
        let winid = b:GetList(#{filewinid: 0}).filewinid
        if winid == 0
            call utils#echoError('Parent window missing')
            return
        endif
        call win_execute(winid, 'call ctx.reload_func()')
    else
        call ctx.reload_func()
    endif
    let title = b:GetList(#{title: 0}).title
    let statusmsg = printf('[%s] %s', strftime('%H:%M:%S'), title)
    call utils#echo(utils#shortenCmdline(statusmsg))
endfunction

command! -buffer -bar QfReload call s:QfReload()
nnoremap <buffer> <Leader>rr <Cmd>QfReload<CR>

" ----------------------------------------

let &cpo = s:cpo_save
let b:did_ftplugin = 1
