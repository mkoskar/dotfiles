" vim: fdm=marker

if exists('did_load_filetypes') | finish | endif
let s:cpo_save = &cpo
set cpo&vim

" ----------------------------------------

augroup filetypedetect
    autocmd BufNewFile,BufRead *.gradle setf groovy

    autocmd BufNewFile,BufRead *.sls setf yaml

    autocmd BufNewFile,BufRead *.yaml,*.yml
        \  if !empty(findfile('ansible.cfg', '.;'))
        \|    setf yaml.ansible
        \| endif

    autocmd BufNewFile,BufRead .msmtprc*,msmtprc* setf msmtp

    autocmd BufNewFile,BufRead .tmux.conf*,tmux.conf* setf tmux

    autocmd BufNewFile,BufRead /usr/share/zsh/*/help/* setf man

    autocmd BufNewFile,BufRead bash-fc-* setf sh

    autocmd BufNewFile,BufRead ~/.inputrc.* setf readline
augroup END

" ----------------------------------------

let &cpo = s:cpo_save
