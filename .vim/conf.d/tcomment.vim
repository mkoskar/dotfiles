" vim: fdm=marker

packadd! tcomment

let tcomment#options = #{whitespace: 'no'}
let tcomment_mapleader1 = ''
let tcomment_mapleader2 = ''

nnoremap <silent> <Leader>cc :TComment<CR>
nnoremap <silent> <Leader>cC :TCommentBlock<CR>

nnoremap <silent> <Leader>cp m`vip:TComment<CR>``
nnoremap <silent> <Leader>cP m`vip:TCommentBlock<CR>``

xnoremap <silent> <Leader>cc :TComment<CR>
xnoremap <silent> <Leader>cC :TCommentBlock<CR>
