# Source this file for interactive shell post-initialization.
# :Compatibility: POSIX

[ "$SHRC_DEBUG" ] && echo '~/bin/shrc-post.sh' >&2

case $- in *i*) ;; *) return ;; esac

# ----------------------------------------

if [ -d "$PYENV_ROOT" ]; then
    source_opt "$PYENV_ROOT/completions/pyenv.$SHELL_NAME"
fi

source_opt "$HOME/opt/fzf/shell/completion.$SHELL_NAME"
source_opt "$HOME/opt/fzf/shell/key-bindings.$SHELL_NAME"


# Login shell only
# ----------------------------------------

case $- in *l*) ;; *) return ;; esac

if [ "$TTY" = '/dev/tty1' ]; then
    up
fi
