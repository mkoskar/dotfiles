# Source this file for interactive shell pre-initialization.
# :Compatibility: POSIX

[ "$SHRC_DEBUG" ] && echo '~/bin/shrc-pre.sh' >&2

case $- in *i*) ;; *) return ;; esac

# ----------------------------------------

if [ "$BASH_VERSION" ]; then
    SHELL_NAME='bash'
elif [ "$ZSH_VERSION" ]; then
    SHELL_NAME='zsh'
else
    SHELL_NAME='sh'
fi

CDPATH='.:..:~'
PS4=$'+\n$ti_hi1>$ti_reset [$(date +%T)] $ti_hi3$BASH_SOURCE:$LINENO$ti_reset\n$ti_hi1>$ti_reset $ti_hi0$BASH_COMMAND$ti_reset\n$ti_hi1>$ti_reset '
unset MAILCHECK

if [ -e /usr/lib/libstderred.so ]; then
    alias stderred="LD_PRELOAD=/usr/lib/libstderred.so\${LD_PRELOAD:+:\$LD_PRELOAD}"
    STDERRED_ESC_CODE=$(tput setaf 3)
    export STDERRED_ESC_CODE
fi
