# Source this file to initialize terminal.
# :Compatibility: POSIX

unset TERMDONE

[ ! -t 0 ] && return

TTY=$(tty)
export TTY

[ "${TTYDONE-}" = "$TTY" ] && return

export GPG_TTY=$TTY
export TERMORIG=$TERM

if [ "${TERM%%-*}" = 'screen' ]; then
    export TERM=screen-256color
fi

stty -ixon -ixoff

export TERMDONE=1
export TTYDONE=$TTY
