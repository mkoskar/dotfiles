" vim: fdm=marker

packadd! dirvish

let dirvish_hijack_netrw = 0
let dirvish_mode = ':sort | sort ,^.*/,'

nmap <M-u> <Plug>(dirvish_up)

augroup dirvish_conf
    autocmd!
    autocmd FileType dirvish call utils#bufSpecial()
    autocmd FileType dirvish nmap <buffer> <silent> q <Plug>(dirvish_quit)
augroup END
