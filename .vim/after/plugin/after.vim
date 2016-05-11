" vim: ft=vim fdm=marker

nnoremap col :setl <C-R>=!empty(&cc) ? 'cc=' : 'cc='.option_colorcolumn<CR><CR>
nnoremap cov :setl <C-R>=&virtualedit =~# 'all' ? 'virtualedit=block' : 'virtualedit=all'<CR><CR>

nnoremap <silent> <Leader>K :cNext<CR>

if exists(':PatienceDiff')
    PatienceDiff
endif
