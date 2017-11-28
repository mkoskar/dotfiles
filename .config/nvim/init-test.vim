" vim: ft=vim fdm=marker

set nocompatible
set noloadplugins

let mapleader = ','

set laststatus=2
set noruler
set noshowcmd
set shortmess+=I
set signcolumn=yes
set viminfo=

"if !has('nvim')
"    set t_ut=
"endif

colorscheme default

tabnew

function! Test() abort
    let themes = ['default', 'desert', 'blue', 'evening']
    let l = len(themes)
    for theme in themes
        exec 'colorscheme '.theme
        let c = 0
        while c < 30
            tabnext
            redraw
            sleep 200 m
            let c += 1
        endwhile
    endfor
    echomsg 'Test: finished - '.(has('nvim') ? 'NVIM' : 'VIM')
endfunction

nnoremap <silent> <Leader>tt :call Test()<CR>
nnoremap <silent> <C-L> :redraw<CR>
nnoremap <silent> Q :qa!<CR>

autocmd! VimEnter * call Test()
