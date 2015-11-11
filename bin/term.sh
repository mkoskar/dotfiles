# Source this file to initialize terminal.
# :Compatibility: POSIX

[ ! -t 0 ] && return

export TTY=$(tty)

[ "$TERMDONE" = "$TTY" ] && return

export GPG_TTY=$TTY
export TERMORIG=$TERM

if [ "${TERM%%-*}" = 'screen' ]; then
    export TERM='screen-256color'
fi

tput reset

# turn off flow control
stty -ixon -ixoff

if [ "$TERM" = 'linux' -o \
     "$TERM" = 'fbterm' ]; then
    {
        printf '\e]P0000000'
        printf '\e]P19d2d2d'
        printf '\e]P245a345'
        printf '\e]P3db9c2e'
        printf '\e]P4384580'
        printf '\e]P5a43fa4'
        printf '\e]P6229f9f'
        printf '\e]P7aeaeae'
        printf '\e]P84d4d4d'
        printf '\e]P9c04f4f'
        printf '\e]PA70c066'
        printf '\e]PBd4c668'
        printf '\e]PC606cb5'
        printf '\e]PDbe63be'
        printf '\e]PE68bbbb'
        printf '\e]PFdddddd'
    } >/dev/tty
fi

export TERMDONE=$TTY
