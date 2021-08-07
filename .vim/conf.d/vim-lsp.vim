" vim: fdm=marker

packadd! vim-lsp
packadd! vim-lsp-settings

let lsp_diagnostics_echo_cursor = 1
let lsp_diagnostics_echo_delay = 100
let lsp_diagnostics_highlights_delay = 100
let lsp_diagnostics_signs_delay = 100
let lsp_diagnostics_signs_error = {'text': '>'}
let lsp_diagnostics_signs_hint = {'text': '>'}
let lsp_diagnostics_signs_information = {'text': '>'}
let lsp_diagnostics_signs_warning = {'text': '>'}
let lsp_diagnostics_virtual_text_delay = 100
let lsp_diagnostics_virtual_text_insert_mode_enabled = 1
let lsp_diagnostics_virtual_text_prefix = ' ■ '
let lsp_document_code_action_signs_delay = 100
let lsp_document_code_action_signs_hint = {'text': '‼'}
let lsp_document_highlight_delay = 100
let lsp_format_sync_timeout = 5000
let lsp_log_file = $VIMDIR . '/vim-lsp.log'
let lsp_show_message_log_level = 'log'
let lsp_show_workspace_edits = 1
let lsp_signature_help_delay = 100

let lsp_settings_enable_suggestions = 0

function! s:on_lsp_buffer_enabled() abort
    setl omnifunc=lsp#complete
    setl tagfunc=lsp#tagfunc

    nmap <buffer> gD <Plug>(lsp-declaration)
    nmap <buffer> gd <Plug>(lsp-definition)

    nmap <LocalLeader>fo <Plug>(lsp-document-range-format)
    xmap <LocalLeader>fo <Plug>(lsp-document-range-format)

    nmap <LocalLeader>; <Plug>(lsp-hover)
    nmap <LocalLeader>J <Plug>(lsp-next-error-nowrap)
    nmap <LocalLeader>K <Plug>(lsp-previous-error-nowrap)
    nmap <LocalLeader>ca <Plug>(lsp-code-action)
    nmap <LocalLeader>ci <Plug>(lsp-call-hierarchy-incoming)
    nmap <LocalLeader>cl <Plug>(lsp-code-lens)
    nmap <LocalLeader>co <Plug>(lsp-call-hierarchy-outgoing)
    nmap <LocalLeader>ds <Plug>(lsp-document-symbol)
    nmap <LocalLeader>i <Plug>(lsp-implementation)
    nmap <LocalLeader>j <Plug>(lsp-next-diagnostic-nowrap)
    nmap <LocalLeader>k <Plug>(lsp-previous-diagnostic-nowrap)
    nmap <LocalLeader>ll <Plug>(lsp-document-diagnostics)
    nmap <LocalLeader>rf <Plug>(lsp-references)
    nmap <LocalLeader>rj <Plug>(lsp-next-reference)
    nmap <LocalLeader>rk <Plug>(lsp-previous-reference)
    nmap <LocalLeader>rn <Plug>(lsp-rename)
    nmap <LocalLeader>s <Plug>(lsp-signature-help)
    nmap <LocalLeader>td <Plug>(lsp-type-definition)
    nmap <LocalLeader>th <Plug>(lsp-type-hierarchy)
    nmap <LocalLeader>wa <Nop>
    nmap <LocalLeader>wl <Nop>
    nmap <LocalLeader>wr <Nop>
    nmap <LocalLeader>ws <Plug>(lsp-workspace-symbol)
endfunction

augroup vim_lsp_conf
    autocmd!
    autocmd User lsp_buffer_enabled call s:on_lsp_buffer_enabled()
augroup END
