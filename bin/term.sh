# Source this file to initialize terminal.
# :Compatibility: POSIX

[ ! -t 0 ] && return

export TTY=$(tty)

[ "$TERMDONE" = "$TTY" ] && return

export GPG_TTY=$TTY

if [ ! "${TERM%%-*}" = 'screen' ]; then
    # not screen
    export TERMOLD=$TERM
else
    # inside screen
    case "_$TERMOLD" in
        _)
            export TERMOLD=$TERM
            ;;
        _linux)
            export TERM='screen.linux'
            ;;
        *)
            export TERM='screen'
            if [ "$(tput "-T$TERMOLD" colors)" -eq 256 ]; then
                export TERM='screen-256color'
            fi
            ;;
    esac

    #case $TERM in screen | screen-256color)
    #    if infocmp "$TERM-bsdel" >/dev/null 2>&1 && \
    #        [ "$(stty -g | awk -F ':' '{print $7}')" = '7f' ]; then
    #        export TERM="$TERM-bsdel"
    #    fi
    #    ;;
    #esac
fi

# turn off flow control
stty -ixon -ixoff

export TERMDONE=$TTY
