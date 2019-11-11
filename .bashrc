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

__ti_tsl=$(tput tsl)
__ti_fsl=$(tput fsl)
[[ $__ti_tsl ]] && __title="\\[$__ti_tsl\u@\h:\w$__ti_fsl\\]"
PS1="$__title\$__statstr:\${BASEDIR:+(\${BASEDIR##*/}):}\\W\$ "
if [[ $HOSTNAME != 'mirci' ]]; then
    PS1="$__title\$__statstr:\\h:\${BASEDIR:+(\${BASEDIR##*/}):}\\W\$ "
fi

__preexec() {
    __cmd=$BASH_COMMAND
    __cmd_start=$EPOCHSECONDS
}

__prompt_command() {
    local pstatus=($? "${PIPESTATUS[@]}") __cmd_dur
    __statstr=${pstatus[0]}
    if (( ${#pstatus[@]} > 2 )); then
        __statstr+=:$(IFS=\|; echo "${pstatus[*]:1}")
    fi
    __cmd_dur=$((EPOCHSECONDS-__cmd_start))
    # shellcheck disable=SC2034
    if (( __cmd_dur > 10 )); then
        __long_cmd=$__cmd
        __long_cmd_start=$__cmd_start
        __long_cmd_dur=$__cmd_dur
    fi
    history -a
    trap __preexec DEBUG
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
complete -o nospace -f paco
complete -o nospace -v v

_pacpkgs() {
    COMPREPLY=()
    . /usr/share/bash-completion/completions/pacman
    # shellcheck disable=SC2034
    local cur prev
    _get_comp_words_by_ref cur prev
    _pacman_pkg Qq
}
complete -o nospace -F _pacpkgs \
    paccheck pacd paci pacl pacp pacr pacscripts pactree pacw

# ----------------------------------------

. ~/bin/shrc-post.sh
