# Source this file for interactive shell post-initialization.

[ "$SHRC_DEBUG" ] && echo \~/bin/shrc-post.sh >&2

case $- in *i*) ;; *) return ;; esac

# ----------------------------------------

source_opt /usr/share/fzf/completion."$SHMODE"
source_opt /usr/share/fzf/key-bindings."$SHMODE"

eval "$(register-python-argcomplete pipx)"


# Login shell only
# ----------------------------------------

case $- in *l*) ;; *) return ;; esac

if [ "$TTY" = /dev/tty1 ]; then
    up
fi
