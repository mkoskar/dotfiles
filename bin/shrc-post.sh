# Source this file for interactive shell post-initialization.

[ "$SHRC_DEBUG" ] && echo \~/bin/shrc-post.sh >&2

case $- in *i*) ;; *) return ;; esac

# ----------------------------------------

source_opt "$SYSPREFIX/share/fzf/completion.$SHMODE"
source_opt "$SYSPREFIX/share/fzf/key-bindings.$SHMODE"

case $SHNAME in bash | zsh)
    if command -v register-python-argcomplete >/dev/null; then
        eval "$(register-python-argcomplete pipx)"
    fi
esac


# Login shell only
# ----------------------------------------

case $- in *l*) ;; *) return ;; esac

if [ "$TTY" = /dev/tty1 ]; then
    up
fi
