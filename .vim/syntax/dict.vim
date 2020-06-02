" Vim syntax file
" Language: DICT protocol

if exists('b:current_syntax') | finish | endif
let s:cpo_save = &cpo
set cpo&vim

" ----------------------------------------

syn case ignore

syn sync fromstart
set foldmethod=syntax

syn match dictResponse /^\d\+ .*$/
syn region dictDefinitionBody matchgroup=dictDefinitionHeader start=+^151 .*$+
    \ matchgroup=dictDefinitionFooter end=+^\.$+ contains=CONTAINED keepend fold

syn region dictTerm start=+\\+ end=+\\,+he=e-1 oneline contained
syn region dictTerm start=+\\.*\\+ end=+(.*),+he=e-1 oneline contained
syn region dictString start=+"+ end=+"+ contained
syn region dictReference start=+{+ end=+}+ contained
syn region dictAnnotation start=+\[+ end=+\]+ contains=dictAnnotation,dictReference contained

hi def link dictResponse Comment
hi def link dictDefinitionHeader ErrorMsg
hi def link dictDefinitionFooter Comment

hi def link dictTerm Identifier
hi def link dictString PreProc
hi def link dictReference Statement
hi def link dictAnnotation Constant

" ----------------------------------------

let &cpo = s:cpo_save
let b:current_syntax = 'dict'
