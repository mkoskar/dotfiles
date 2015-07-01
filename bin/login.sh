# Source this file to finalize interactive login shell initialization.
# :Compatibility: POSIX

[ -t 0 ] || return

if [ "$TTY" = '/dev/tty1' ]; then
    up
fi
