" vim: fdm=marker

if exists('b:current_syntax') | finish | endif
let s:cpo_save = &cpo
set cpo&vim

" ----------------------------------------

syn case ignore
syn sync fromstart

syn match ssIPv4 /\(\d\{1,3}\.\)\{3}\d\{1,3}\(\/\d\+\)\?/
syn match ssIPv6 /[[:xdigit:]:]*::[[:xdigit:]:]*\(\/\d\+\)\?/
syn match ssIPv6FQ /\(\x\{1,4}:\)\{7}\x\{1,4}\(\/\d\+\)\?/

hi def link ssIPv4 Constant
hi def link ssIPv6 Constant
hi def link ssIPv6FQ Constant

" ----------------------------------------

let &cpo = s:cpo_save
let b:current_syntax = 'ss'
