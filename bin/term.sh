# Source this file to set correct terminal.

if [ "$TERM" = linux ]; then
    # linux console supports 16 colors
    export TERM='linux-16color'
fi

if [[ ! "$TERM" =~ ^screen ]]; then
    export TERMOLD=$TERM
    return
fi

case "$TERMOLD" in
    linux | linux-16color)
        export TERM="screen.$TERMOLD"
        return
        ;;
esac

export TERM='screen'

TERMOLD_COLORS=$(tput -T$TERMOLD colors)
if [ $TERMOLD_COLORS == 256 ]; then
    export TERM='screen-256color'
fi

VERASE=$(stty -g | awk -F ':' '{ print $7 }')
if [ $VERASE == 7f ]; then
    export TERM="$TERM-bsdel"
fi
