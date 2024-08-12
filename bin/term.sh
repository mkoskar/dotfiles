# Source this file to initialize terminal.

unset TERMDONE

[ ! -t 0 ] && return

TTY=$(tty)
export TTY

[ "${TTYDONE-}" = "$TTY" ] && return

export GPG_TTY=$TTY
export TERMORIG=$TERM

stty -ixon -ixoff

export TERMDONE=1
export TTYDONE=$TTY
