" vim: fdm=marker

packadd! ale

function! AleStatus() abort
    try
        let total = ale#statusline#Count(bufnr('%')).total
        return total == 0 ? '' : printf('[%d]', total)
    finally
        return ''
    endtry
endfunction

set statusline+=%(\ %#ErrorMsg#%{AleStatus()}%*%)

let ale_echo_cursor = 1
let ale_echo_msg_format = '[%linter%] %code%: %s'
let ale_lint_on_enter = 1
let ale_lint_on_insert_leave = 1
let ale_lint_on_text_changed = 'normal'
let ale_sign_error = '>'
let ale_sign_warning = '>'

let ale_python_flake8_options = '--ignore=E265,E266'
let ale_sh_shellcheck_exclusions = 'SC1003,SC1007,SC1083,SC2088,SC2191'

nnoremap <silent> <C-Space> :ALEDetail<CR>
nmap <Leader>aa <Plug>(ale_toggle)
nmap <Leader>ag <Plug>(ale_first)
nmap <Leader>aj <Plug>(ale_next)
nmap <Leader>ak <Plug>(ale_previous)
nmap <Leader>aG <Plug>(ale_last)
