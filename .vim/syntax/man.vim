" Vim syntax file
" Language: Man page
" based on /usr/share/vim/vim81/syntax/man.vim

if exists('b:current_syntax') | finish | endif
let s:cpo_save = &cpo
set cpo&vim

" ----------------------------------------

syn case ignore

runtime! syntax/ctrlh.vim

syn match manReference "\f\+([1-9][a-z]\=)"
syn match manTitle "^\f\+([0-9]\+[a-z]\=).*"

syn match manSectionHeading "^[a-z][a-z -]*[a-z]$"
syn match manSubHeading "^\s\{3\}[a-z][a-z -]*[a-z]$"

syn match manOptionDesc "^\s*[+-][a-z0-9]\S*"
syn match manLongOptionDesc "^\s*--[a-z0-9-]\S*"

hi def link manReference PreProc
hi def link manTitle Title

hi def link manSectionHeading Statement
hi def link manSubHeading Function

hi def link manOptionDesc Constant
hi def link manLongOptionDesc Constant

" ----------------------------------------

let &cpo = s:cpo_save
let b:current_syntax = 'man'
