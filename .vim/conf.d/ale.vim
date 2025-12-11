" vim: fdm=marker

packadd! ale
packadd! vim-lsp-ale

function! AleStatus() abort
    if !g:ale_enabled | return '[-]' | endif
    let total = ale#statusline#Count(bufnr('%')).total
    return printf('[%d]', total)
endfunction

"let &g:stl .= '%( %{AleStatus()}%*%)'
"let &g:stl .= '%( %1*%{AleStatus()}%*%)'
let &g:stl .= '%( %#ErrorMsg#%{AleStatus()}%*%)'

"let ale_completion_delay = 10
"let ale_completion_enabled = 1
"let ale_linters_explicit = 1
let ale_echo_cursor = 1
let ale_echo_msg_format = '%severity%: [%linter%] %code: %%s'
let ale_floating_preview = 1
let ale_floating_window_border = []
let ale_loclist_msg_format = '[%linter%] %code: %%s'
let ale_lsp_show_message_format = '%severity%: [%linter%] %s'
let ale_lsp_show_message_severity = 'information'
let ale_lsp_suggestions = 1
let ale_set_loclist = 0
let ale_shell = 'bash'
let ale_sign_error = '>'
let ale_sign_warning = '>'

let ale_linters = {}
let ale_linters.c = ['clangd']
let ale_linters.golang = ['gofmt', 'gopls']
let ale_linters.javascript = ['tsserver']
let ale_linters.python = ['bandit', 'flake8', 'pyright']
let ale_linters.rust = ['analyzer']
let ale_linters.sh = ['shellcheck']

let ale_fixers = {}
let ale_fixers.golang = ['gofmt']
let ale_fixers.javascript = ['eslint']
let ale_fixers.python = ['autopep8', 'trim_whitespace']

let ale_python_flake8_options = '--ignore=E265,E266,E501'
"let ale_sh_shellcheck_exclusions = 'SC1003,SC1007,SC1083,SC2088,SC2191'

function! s:ALESetLocList(action) abort
    let items = ale#engine#GetLoclist(bufnr('%'))
    if empty(items) | return | endif
    call setloclist(0, [], a:action, #{
        \ nr: a:action == ' ' ? '$' : 0,
        \ items: items,
        \ title: ':ALESetLocList',
        \ quickfixtextfunc: 0,
        \ context: #{reload_func: function('s:ALESetLocList', ['r'])},
        \ })
    lopen
endfunction

command! -bang -bar ALESetLocList call s:ALESetLocList(empty(<q-bang>) ? ' ' : 'r')
nmap <Leader>al <Cmd>ALESetLocList<CR>

nmap <C-Space> <Plug>(ale_detail)
nmap <Leader>aa <Plug>(ale_toggle)
nmap <Leader>ag <Plug>(ale_first)
nmap <Leader>aj <Plug>(ale_next)
nmap <Leader>ak <Plug>(ale_previous)
nmap <Leader>aG <Plug>(ale_last)
