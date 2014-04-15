# Source this file to set correct terminal.
# :Compatibility: POSIX

if [ -n "${TERM##screen*}" ]; then
    export TERMOLD="$TERM"
    return
fi

case "$TERMOLD" in
    linux | linux-16color)
        export TERM="screen.$TERMOLD"
        return
        ;;
esac

export TERM='screen'

if [ "$(tput "-T$TERMOLD" colors)" -eq 256 ]; then
    export TERM='screen-256color'
fi

if [ "$(stty -g | awk -F ':' '{ print $7 }')" = '7f' ]; then
    export TERM="$TERM-bsdel"
fi
