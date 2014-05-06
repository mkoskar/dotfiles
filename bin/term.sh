# Source this file to initialize terminal.
# :Compatibility: POSIX

if [ -n "${TERM##screen*}" ]; then
    # not screen
    export TERMOLD="$TERM"
else
    # inside screen
    case "$TERMOLD" in
        linux)
            export TERM='screen.linux'
            ;;
        *)
            export TERM='screen'
            if [ "$(tput "-T$TERMOLD" colors)" -eq 256 ]; then
                export TERM='screen-256color'
            fi
            if [ "$(stty -g | awk -F ':' '{ print $7 }')" = '7f' ]; then
                export TERM="$TERM-bsdel"
            fi
            ;;
    esac
fi

# turn off flow control
stty -ixon -ixoff
