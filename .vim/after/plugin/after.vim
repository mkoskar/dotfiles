" vim: ft=vim fdm=marker

nnoremap col :setl <C-R>=!empty(&cc) ? 'cc=' : 'cc='.option_colorcolumn<CR><CR>
nnoremap <silent> <Leader>K :cNext<CR>
