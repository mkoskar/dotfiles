" vim: fdm=marker

packadd! easymotion

let EasyMotion_smartcase = 1
let EasyMotion_startofline = 0
let EasyMotion_verbose = 0

map <Leader><Leader>/ <Plug>(easymotion-sn)
map <Leader><Leader>J <Plug>(easymotion-sol-j)
map <Leader><Leader>K <Plug>(easymotion-sol-k)
map <Leader>; <Plug>(easymotion-next)
map <Leader>\ <Plug>(easymotion-prev)

hi EasyMotionTarget ctermfg=3 cterm=bold
hi link EasyMotionIncCursor Cursor
hi link EasyMotionIncSearch IncSearch
hi link EasyMotionMoveHL IncSearch
hi link EasyMotionShade Comment
hi link EasyMotionTarget2First EasyMotionTarget
hi link EasyMotionTarget2Second EasyMotionTarget
