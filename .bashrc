# ~/.bashrc
# - executed by bash(1) for interactive non-login shells

[ -e ~/bin/term.sh ] && . ~/bin/term.sh

# continue only in case of interactive shell
# ------------------------------------------
case $- in *i*) ;; *) return ;; esac

[ -e ~/bin/shx.sh ] && . ~/bin/shx.sh

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

_title='\[\e]0;\u@\h:\w\a\]'
PS1="$_title\$?:\W\$ "
if [ -n "$(hostname-label)" ]; then
    PS1="$_title\$?:\h:\W\$ "
fi

complete -o nospace -W '10m 15m 20m 25m 30m' a
complete -o nospace -c pth
complete -o nospace -c spth
complete -o nospace -c on
complete -o nospace -f paco
complete -o nospace -c pacoc

_pacl() {
    COMPREPLY=()
    . /usr/share/bash-completion/completions/pacman
    local cur prev
    _get_comp_words_by_ref cur prev
    _pacman_pkg Qq
}
complete -o nospace -F _pacl pacl pacd pacp pacw paci
