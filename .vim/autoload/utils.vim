" vim: ft=vim fdm=marker

function! utils#EchoWarn(msg) abort
    if !empty(a:msg)
        redraw
        echohl WarningMsg | echomsg a:msg | echohl None
    endif
endfunction

function! utils#EchoError(msg) abort
    if !empty(a:msg)
        redraw
        echohl ErrorMsg | echomsg a:msg | echohl None
    endif
endfunction

function! utils#EchoException() abort
    call utils#EchoError(substitute(v:exception, '^Vim(.*):', '', 'g'))
endfunction

" ----------------------------------------

function! utils#BufSpecial() abort
    setl nonu nornu nowrap nolist nobl cc=0 fdc=0
endfunction

function! utils#CmdlineWDelete() abort
    let buf = getcmdline()
    let pos = getcmdpos()
    let idx = match(buf, '^\@<!\<', pos)
    return strpart(buf, 0, pos-1).(idx < 0 ? '' : strpart(buf, idx))
endfunction

function! utils#PluginAdd(name) abort
    let path = a:name
    if match(path, '\v^(/|\~/|\./)') < 0
        let path = '~/.vim/bundle/'.path
    endif
    let path = fnamemodify(path, ':p')
    call pathogen#surround(path)
    return path
endfunction

function! utils#PluginLoad(name) abort
    let path = utils#PluginAdd(a:name)
    exec 'source '.fnameescape(path).'/plugin/*.vim'
endfunction

function! utils#TryCatch(cmd, ...) abort
    let pattern = get(a:, '1', '.*')
    exec 'try | exec "'.escape(a:cmd, '"')
        \ '" | catch /'.pattern.'/ | call utils#EchoException() | endtry'
endfunction
