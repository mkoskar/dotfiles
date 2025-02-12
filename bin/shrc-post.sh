# Source this file for interactive shell post-initialization.

[ "$SHRC_DEBUG" ] && echo ~/bin/shrc-post.sh >&2

case $- in *i*) ;; *) return ;; esac

# ----------------------------------------

source_opt "$SYSPREFIX/share/fzf/completion.$SHMODE"
source_opt "$SYSPREFIX/share/fzf/key-bindings.$SHMODE"


# Login shell only
# ----------------------------------------

case $- in *l*) ;; *) return ;; esac

case $OSID in
    termux)
        ~/.termux/runsvdir >/dev/null 2>&1
        #ssh-agent-preset
        ;;
    *)
        [ "$(tty)" = /dev/tty1 ] || { return 0; }

        #echo
        #echo --------------------------------------------------
        #echo '> Setup backup:'
        #echo
        #sudo backup-setup

        #echo
        #echo --------------------------------------------------
        #echo '> Presets:'
        #echo
        #gpg-agent-preset
        #ssh-agent-preset

        echo
        echo --------------------------------------------------
        echo '> Start X session:'
        echo
        confirm 'Continue?' y || { echo; return; }
        exec x
        ;;
esac
