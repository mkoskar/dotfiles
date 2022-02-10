" vim: fdm=marker

function! utils#echo(msg) abort
    if !empty(a:msg)
        echohl None | echo a:msg
    endif
endfunction

function! utils#echoWarn(msg) abort
    if !empty(a:msg)
        echohl WarningMsg | echo a:msg | echohl None
    endif
endfunction

function! utils#echoError(msg) abort
    if !empty(a:msg)
        echohl ErrorMsg | echomsg a:msg | echohl None
    endif
endfunction

function! utils#echoException() abort
    call utils#echoError(substitute(v:exception, '^Vim\%((\a\+)\)\=:', '', 'g'))
endfunction

" ----------------------------------------

function! utils#cmdlineKillWord(forward, ...) abort
    let buf = getcmdline()
    let pos = getcmdpos() - 1
    let idx = utils#cmdlineMatchWord(a:forward, get(a:, 1, 0))
    if a:forward
        return strpart(buf, 0, pos) . (idx < 0 ? '' : strpart(buf, idx))
    else
        call setcmdpos(idx + 1)
        return strpart(buf, 0, idx) . strpart(buf, pos)
    endif
endfunction

function! utils#cmdlineMatchWord(forward, emacs) abort
    let buf = getcmdline()
    let pos = getcmdpos() - 1
    if a:forward
        if a:emacs
            return match(buf, '\v>|$', pos)
        else
            return match(buf, '\v^@<!<|$', pos)
        endif
    else
        return match(strpart(buf, 0, pos), '\v(.*\zs<|^)')
    endif
endfunction

function! utils#cmdlineMoveWord(forward, ...) abort
    let idx = utils#cmdlineMatchWord(a:forward, get(a:, 1, 0))
    call setcmdpos(idx + 1)
    return getcmdline()
endfunction

function! utils#moveWord(forward, ...) abort
    if a:forward
        if get(a:, 1, 0)
            call search('\v(.{-1,}\zs>)|(%#@<!$)', 'cWz')
        else
            call search('\v(.{-1,}\zs<)|(%#@<!$)', 'cWz')
        endif
    else
        call search('\v<|^', 'bW')
    endif
endfunction

" ----------------------------------------

function! utils#bufPlain() abort
    setl nonu nornu nowrap nolist cc= scl=no
endfunction

function! utils#bufSpecial() abort
    call utils#bufPlain()
    setl fdc=0
endfunction

function! utils#mkdir(path) abort
    if !isdirectory(a:path)
        call mkdir(a:path, 'p', 0700)
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

function! utils#tryCatchCall(func, ...) abort
    try
        let Fn = function(a:func, a:000)
        call Fn()
    catch /^\(Vim:Interrupt\)\@!.*/
        call utils#echoException()
    endtry
endfunction

function! utils#tryCatchCallRange(func, ...) range abort
    try
        let Fn = function(a:func, a:000)
        exec a:firstline ',' a:lastline 'call Fn()'
    catch /^\(Vim:Interrupt\)\@!.*/
        call utils#echoException()
    endtry
endfunction
