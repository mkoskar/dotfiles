" vim: fdm=marker

packadd! deoplete

let deoplete#enable_at_startup = 1
let deoplete#enable_smart_case = 1

"inoremap <expr> <C-G> deoplete#undo_completion()
"inoremap <expr> <C-L> deoplete#refresh()
"inoremap <expr> <CR> deoplete#close_popup() . '\<CR>'
