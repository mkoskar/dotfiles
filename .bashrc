# ~/.bashrc
# - executed by bash(1) for interactive non-login shells

[ -f ~/bin/shrc.sh ] && . ~/bin/shrc.sh

# continue only in case of interactive shell
# ------------------------------------------
case $- in *i*) ;; *) return ;; esac

shopt -s autocd checkjobs checkwinsize cmdhist dotglob histappend \
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

CDPATH='.:..:~'
PROMPT_DIRTRIM=3
PS1='$?:\W\$ '

complete -o nospace -W '10m 15m 20m 25m 30m' a
complete -o nospace -c pth
complete -o nospace -c spth
complete -o nospace -c on
complete -o nospace -f paco
complete -o nospace -c pacoc

. /usr/share/bash-completion/completions/man
complete -F _man manl

_pacl() {
    COMPREPLY=()
    . /usr/share/bash-completion/completions/pacman
    local cur prev
    _get_comp_words_by_ref cur prev
    _pacman_pkg Qq
}
complete -o nospace -F _pacl pacl pacd pacp pacw paci
