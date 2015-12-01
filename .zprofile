# Executed by zsh(1) for login shells.

[[ $SHRC_DEBUG ]] && echo '~/.zprofile' >&2

. ~/.profile

{
    zcompdump="${ZDOTDIR:-$HOME}/.zcompdump"
    if [[ -e "$zcompdump" && (! -f "${zcompdump}.zwc" ||
            "$zcompdump" -nt "${zcompdump}.zwc") ]]; then
        zcompile "$zcompdump"
    fi
} &!
