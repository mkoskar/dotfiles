# Executed by bash(1) for interactive non-login shells.

[[ $SHRC_DEBUG ]] && echo \~/.bashrc >&2

. ~/bin/term.sh

# ----------------------------------------

case $- in *i*) ;; *) return ;; esac

. ~/bin/shx.sh
. ~/bin/shrc-pre.sh

HISTCONTROL=ignorespace:erasedups
HISTFILE=~/.local/share/bash_history
HISTFILESIZE=5000
HISTIGNORE=exit
HISTSIZE=1000

__title="\\[$(hstatus '\u@\h:\w')\\]"
PS1="$__title\$?\$__statstr:\${BASEDIR:+(\${BASEDIR##*/}):}\\W\$ "
if [[ $HOSTNAME != 'mirci' ]]; then
    PS1="$__title\$?\$__statstr:\\h:\${BASEDIR:+(\${BASEDIR##*/}):}\\W\$ "
fi

__prompt_command() {
    local pstatus=("${PIPESTATUS[@]}")
    __statstr=
    if (( ${#pstatus[@]} > 1 )); then
        __statstr=:${pstatus[0]}$(printf '|%d' "${pstatus[@]:1}")
    fi
    history -a
}
PROMPT_COMMAND=__prompt_command

shopt -s autocd checkjobs checkwinsize cmdhist dotglob gnu_errfmt histappend \
         histreedit histverify lithist no_empty_cmd_completion

# some can't detect editing-mode set in ~/.inputrc early enough (e.g., fzf)
set -o vi

complete -o nospace -A function fn
complete -o nospace -c i
complete -o nospace -c on
complete -o nospace -c pacoc
complete -o nospace -c watch
complete -o nospace -f paco
complete -o nospace -v v

_pacl() {
    COMPREPLY=()
    . /usr/share/bash-completion/completions/pacman
    # shellcheck disable=SC2034
    local cur prev
    _get_comp_words_by_ref cur prev
    _pacman_pkg Qq
}
complete -o nospace -F _pacl pacl pacd pacp pacw paci paccheck pacscripts

# ----------------------------------------

. ~/bin/shrc-post.sh
