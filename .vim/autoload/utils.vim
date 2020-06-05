" vim: fdm=marker

function! utils#echoError(msg) abort
    if !empty(a:msg)
        echohl ErrorMsg | echomsg a:msg | echohl None
    endif
endfunction

function! utils#echoException() abort
    call utils#echoError(substitute(v:exception, '^Vim\%((\a\+)\)\=:', '', 'g'))
endfunction

function! utils#echoWarn(msg) abort
    if !empty(a:msg)
        redraw
        echohl WarningMsg | echo a:msg | echohl None
    endif
endfunction

function! utils#echo(msg) abort
    if !empty(a:msg)
        redraw
        echohl None | echo a:msg
    endif
endfunction

" ----------------------------------------

function! utils#bufSpecial() abort
    setl nonu nornu nowrap nolist nobl cc= fdc=0 signcolumn=no
endfunction

function! utils#cmdlineKillWord(forward) abort
    let buf = getcmdline()
    let pos = getcmdpos() - 1
    let idx = utils#moveWord(buf, pos, a:forward)
    if a:forward
        return strpart(buf, 0, pos) . (idx < 0 ? '' : strpart(buf, idx))
    else
        call setcmdpos(idx + 1)
        return strpart(buf, 0, idx) . strpart(buf, pos)
    endif
endfunction

function! utils#cmdlineMoveWord(forward) abort
    let buf = getcmdline()
    let pos = getcmdpos() - 1
    let idx = utils#moveWord(buf, pos, a:forward)
    call setcmdpos(idx + 1)
    return buf
endfunction

function! utils#mkdir(path) abort
    if !isdirectory(a:path)
        call mkdir(a:path, 'p', 0700)
    endif
endfunction

function! utils#moveWord(buf, pos, forward) abort
    if a:forward
        return match(a:buf, '^\@<!\(\<\|$\)', a:pos)
    else
        return match(strpart(a:buf, 0, a:pos), '\(.*\zs\<\|^\)')
    endif
endfunction

function! utils#rtpAdd(path) abort
    let rtp = split(&rtp, ',')
    let idx = index(rtp, $VIMRUNTIME)
    let path = escape(a:path, '\,')
    call insert(rtp, path . '/after', idx + 1)
    call insert(rtp, path, idx)
    let &rtp = join(rtp, ',')
endfunction

function! utils#rtpPrepend(path) abort
    let path = escape(a:path, '\,')
    let &rtp = join([path, &rtp, path . '/after'], ',')
endfunction

function! utils#shorten(str, len) abort
    if len(a:str) > a:len
        return a:str[:a:len - 1] . '…'
    else
        return a:str
    end
endfunction

function! utils#shortenCmdline(str) abort
    return utils#shorten(a:str, &ch * &co - 35)
endfunction

function! utils#tryCatch(cmd) abort
    try
        exec a:cmd
    catch /^\(Vim:Interrupt\)\@!.*/
        call utils#echoException()
    endtry
endfunction

function! utils#tryCatchCall(func, ...) range abort
    try
        let Fn = function(a:func, a:000)
        exec printf('%d,%d call Fn()', a:firstline, a:lastline)
    catch /^\(Vim:Interrupt\)\@!.*/
        call utils#echoException()
    endtry
endfunction
