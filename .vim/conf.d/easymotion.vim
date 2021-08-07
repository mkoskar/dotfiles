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

hi def link EasyMotionIncCursor Cursor
hi def link EasyMotionIncSearch IncSearch
hi def link EasyMotionMoveHL IncSearch
hi def link EasyMotionShade Comment
hi def link EasyMotionTarget2First EasyMotionTarget
hi def link EasyMotionTarget2Second EasyMotionTarget
