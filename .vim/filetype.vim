" vim: ft=vim fdm=marker

if exists('did_load_filetypes')
    finish
endif

augroup filetypedetect
    autocmd! BufNewFile,BufRead *.gradle setf groovy
    autocmd! BufNewFile,BufRead .msmtprc*,msmtprc* setf msmtp
    autocmd! BufNewFile,BufRead .tmux.conf*,tmux.conf* setf tmux
    autocmd! BufNewFile,BufRead /usr/share/zsh/*/help/* setf man
    autocmd! BufNewFile,BufRead bash-fc-* setf sh
    autocmd! BufNewFile,BufRead ~/.inputrc.* setf readline
augroup END
