" vim: fdm=marker

packadd! ctrlp

let ctrlp_arg_map = 1
let ctrlp_lazy_update = 0
let ctrlp_max_depth = 10
let ctrlp_max_files = 2500
let ctrlp_show_hidden = 1
let ctrlp_working_path_mode = 'rwa'

let ctrlp_prompt_mappings = {
    \ 'PrtCurLeft()': ['<c-h>', '<left>', '<c-^>'],
    \ 'PrtSelectMove("d")': ['<M-j>', '<PageDown>', '<kPageDown>'],
    \ 'PrtSelectMove("u")': ['<M-k>', '<PageUp>', '<kPageUp>'],
    \ }

nnoremap <silent> <M-e> :CtrlPMRUFiles<CR>
