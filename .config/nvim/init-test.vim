" vim: ft=vim fdm=marker

set nocompatible
set noloadplugins

let mapleader = ','

set laststatus=2
set shortmess+=I
set viminfo=

if !has('nvim')
    set t_ut=
endif

colorscheme default

tabnew

function! Test() abort
    let themes = ['default', 'desert', 'blue', 'evening']
    let l = len(themes)
    for theme in themes
        exec 'colorscheme '.theme
        let c = 0
        while c < 30
            redraw!
            sleep 100 m
            let c += 1
        endwhile
    endfor
    echomsg 'Test: finished - '.(has('nvim') ? 'NVIM' : 'VIM')
endfunction

nnoremap <silent> <Leader>tt :call Test()<CR>
nnoremap <silent> Q :qa!<CR>

autocmd! VimEnter * call Test()
