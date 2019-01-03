# Source this file for interactive shell post-initialization.
# :Compatibility: POSIX

[ "$SHRC_DEBUG" ] && echo \~/bin/shrc-post.sh >&2

case $- in *i*) ;; *) return ;; esac

# ----------------------------------------

if [ -d "$PYENV_ROOT" ]; then
    source_opt "$PYENV_ROOT/completions/pyenv.$SHNAME"
fi

source_opt /usr/share/fzf/completion."$SHNAME"
source_opt /usr/share/fzf/key-bindings."$SHNAME"


# Login shell only
# ----------------------------------------

case $- in *l*) ;; *) return ;; esac

if [ "$TTY" = /dev/tty1 ]; then
    up
fi
