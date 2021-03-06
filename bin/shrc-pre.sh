# Source this file for interactive shell pre-initialization.

[ "$SHRC_DEBUG" ] && echo ~/bin/shrc-pre.sh >&2

case $- in *i*) ;; *) return ;; esac

# ----------------------------------------

HISTFILE=~/.local/share/${SHNAME}_history
HISTSIZE=1000

CDPATH='.:..:~'
FCEDIT=fcedit
unset MAILCHECK
unset MAILPATH

if [ -e /usr/lib/libstderred.so ]; then
    alias stderred='LD_PRELOAD=/usr/lib/libstderred.so${LD_PRELOAD:+:$LD_PRELOAD}'
    STDERRED_ESC_CODE=$(tput setaf 3)
    export STDERRED_ESC_CODE
fi
