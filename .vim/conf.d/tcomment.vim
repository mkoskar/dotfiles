" vim: fdm=marker

packadd! tcomment

let tcomment#options = {'whitespace': 'no'}
let tcomment_maps = 0

nnoremap <silent> <Leader>C :TCommentBlock<CR>
nnoremap <silent> <Leader>cc :TComment<CR>
xnoremap <silent> <Leader>C :TCommentBlock<CR>
xnoremap <silent> <Leader>cc :TComment<CR>
