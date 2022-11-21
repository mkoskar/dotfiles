" vim: fdm=marker

if exists('b:current_syntax') | finish | endif
let s:cpo_save = &cpo
set cpo&vim

" ----------------------------------------

setl fdm=syntax cole=3 cocu=nv

syn case ignore
syn sync fromstart

syn region infoBlock start=/^> / end=/\ze\n^-\+/ contains=ALL fold
syn region infoModeline start=/\%1l# vim:/ end=/$/ conceal

hi def link infoModeline Comment

" ----------------------------------------

let &cpo = s:cpo_save
let b:current_syntax = 'info'
