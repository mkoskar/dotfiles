" vim: fdm=marker

if !has('python3') | finish | endif

packadd! ultisnips
packadd! vim-snippets

let UltiSnipsEditSplit = 'vertical'
let UltiSnipsExpandTrigger = '<C-J>'
let UltiSnipsJumpBackwardTrigger = '<C-_>'
let UltiSnipsJumpForwardTrigger = '<C-J>'
let UltiSnipsListSnippets  = '<C-Space>'
