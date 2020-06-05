" vim: fdm=marker

packadd! syntastic

set statusline+=%(\ %#ErrorMsg#%{SyntasticStatuslineFlag()}%*%)

let syntastic_aggregate_errors = 1
let syntastic_always_populate_loc_list = 1
let syntastic_auto_jump = 3
let syntastic_auto_loc_list = 0
let syntastic_check_on_open = 0
let syntastic_error_symbol = '>'
let syntastic_sort_aggregated_errors = 0
let syntastic_stl_format = ' %E{Err: %fe #%e}%B{, }%W{Warn: %fw #%w} '
let syntastic_style_error_symbol = '>'
let syntastic_style_warning_symbol = '>'
let syntastic_warning_symbol = '>'

let syntastic_sh_checkers = ['shellcheck']

nnoremap <silent> <Leader>SS :SyntasticCheck<CR>
nnoremap <silent> <Leader>Si :SyntasticInfo<CR>
nnoremap <silent> <Leader>Sr :SyntasticReset<CR>
