" vim: fdm=marker

packadd! tagbar

let tagbar_autofocus = 1
let tagbar_iconchars = ['▸', '▾']
let tagbar_map_help = ['<F1>']
let tagbar_map_jump = ['<CR>', 'o']
let tagbar_map_togglefold = ['O', 'za']
let tagbar_sort = 0
let tagbar_width = 31

nnoremap <silent> <Leader>Tt :TagbarToggle<CR>
nnoremap <silent> <Leader>To :TagbarOpen j<CR>

hi TagbarHighlight cterm=reverse
hi link TagbarHelp String
