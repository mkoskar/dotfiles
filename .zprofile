# ~/.zprofile
# - executed by zsh(1) for login shells

[ -f ~/.profile ] && . ~/.profile

{
    # compile the completion dump to increase startup speed
    zcompdump="${ZDOTDIR:-$HOME}/.zcompdump"
    if [[ -f "$zcompdump" && (! -f "${zcompdump}.zwc" ||
            "$zcompdump" -nt "${zcompdump}.zwc") ]]; then
        zcompile "$zcompdump"
    fi
} &!
