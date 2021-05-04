# Source this file for interactive shell post-initialization.

[ "$SHRC_DEBUG" ] && echo ~/bin/shrc-post.sh >&2

case $- in *i*) ;; *) return ;; esac

# ----------------------------------------

source_opt "$SYSPREFIX/share/fzf/completion.$SHMODE"
source_opt "$SYSPREFIX/share/fzf/key-bindings.$SHMODE"

case $SHMODE in bash | zsh)
    if command -v register-python-argcomplete >/dev/null; then
        eval "$(register-python-argcomplete pipx)"
    fi
    ;;
esac


# Login shell only
# ----------------------------------------

case $- in *l*) ;; *) return ;; esac

case $OSID in
    termux)
        ~/.termux/runsvdir &>/dev/null
        ssh-agent-preset
        ;;
    *)
        [ "$(tty)" = /dev/tty1 ] || { return 0; }

        echo $'\n--------------------------------------------------'
        echo $'> Setup backup:\n'
        sudo backup-setup

        echo $'\n--------------------------------------------------'
        echo $'> Presets:\n'
        #gpg-agent-preset
        ssh-agent-preset

        echo $'\n--------------------------------------------------'
        echo $'> Start X session:\n'
        confirm 'Continue?' y || { echo; return; }
        exec x
        ;;
esac
