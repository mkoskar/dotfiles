# ~/.zprofile
# - executed by zsh(1) for login shells

[ -e ~/.profile ] && . ~/.profile

{
    # compile the completion dump to increase startup speed
    zcompdump="${ZDOTDIR:-$HOME}/.zcompdump"
    if [[ -e "$zcompdump" && (! -f "${zcompdump}.zwc" ||
            "$zcompdump" -nt "${zcompdump}.zwc") ]]; then
        zcompile "$zcompdump"
    fi
} &!
