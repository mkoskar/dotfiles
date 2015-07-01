# ~/.bashrc
# - executed by bash(1) for interactive non-login shells

[ -n "$SHRC_DEBUG" ] && echo '~/.bashrc' >&2

[ -e ~/bin/term.sh ] && . ~/bin/term.sh

# interactive shell only
# ----------------------------------------

case $- in *i*) ;; *) return ;; esac

[ -e ~/bin/shx.sh ] && . ~/bin/shx.sh

shopt -s autocd checkjobs checkwinsize cmdhist dotglob gnu_errfmt histappend \
         histreedit histverify lithist no_empty_cmd_completion

bind -m vi-insert   '"\C-e": shell-expand-line'
bind -m vi-command  '"\C-e": shell-expand-line'
bind -m vi-insert   '"\ee":  history-and-alias-expand-line'
bind -m vi-command  '"\ee":  history-and-alias-expand-line'
bind -m vi-insert   '"\ei":  complete-filename'
bind -m vi-insert   '"\e\t": dynamic-complete-history'
bind -m vi-insert   '"\eq":  dabbrev-expand'

HISTCONTROL='ignorespace:erasedups'
HISTFILESIZE=5000
HISTIGNORE='exit'
HISTSIZE=500

__title='\[\e]0;\u@\h:\w\a\]'
PS1="$__title\$?\$__statstr:\W\$ "
if [ -n "$(hostname-label)" ]; then
    PS1="$__title\$?\$__statstr:\h:\W\$ "
fi

__prompt_command() {
    local pstatus=("${PIPESTATUS[@]}")
    __statstr=
    if (( ${#pstatus[@]} > 1 )); then
        __statstr=":${pstatus[0]}$(printf '|%d' "${pstatus[@]:1}")"
    fi
}
PROMPT_COMMAND='__prompt_command'

complete -o nospace -A function fn
complete -o nospace -W '10m 15m 20m 25m 30m' a
complete -o nospace -c i
complete -o nospace -c on
complete -o nospace -c pacoc
complete -o nospace -c pgx
complete -o nospace -c pth
complete -o nospace -c ptha
complete -o nospace -c rep
complete -o nospace -c torsocks
complete -o nospace -c xrun
complete -o nospace -c xrun0
complete -o nospace -f paco
complete -o nospace -v v

_pacl() {
    COMPREPLY=()
    . /usr/share/bash-completion/completions/pacman
    local cur prev
    _get_comp_words_by_ref cur prev
    _pacman_pkg Qq
}
complete -o nospace -F _pacl pacl pacd pacp pacw paci pkgmark

# finalize
# ----------------------------------------

[ -e ~/bin/login.sh ] && . ~/bin/login.sh || true
