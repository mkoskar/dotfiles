if has('autocmd')
    augroup filetypedetect
        autocmd BufNewFile,BufRead .msmtprc*,msmtprc* setf msmtp
        autocmd BufNewFile,BufRead .tmux.conf*,tmux.conf* setf tmux
    augroup END
endif
