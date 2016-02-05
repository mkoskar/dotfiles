# Source this file to initialize terminal.
# :Compatibility: POSIX

unset TERMDONE

[ ! -t 0 ] && return

export TTY=$(tty)

[ "${TTYDONE-}" = "$TTY" ] && return

export GPG_TTY=$TTY
export TERMORIG=$TERM

if [ "${TERM%%-*}" = 'screen' ]; then
    export TERM='screen-256color'
fi

tput reset
stty -ixon -ixoff

export TERMDONE=1
export TTYDONE=$TTY
