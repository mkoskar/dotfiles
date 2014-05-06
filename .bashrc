# ~/.bashrc
# - executed by bash(1) for non-login shells

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
HISTSIZE=500
HISTIGNORE='exit'

#PS1='\u@\h:\!:\#:\W\$ '
PS1='\#:\W\$ '
PROMPT_DIRTRIM=3
CDPATH='.:..:~'
unset MAILCHECK

complete -W '10m 15m 20m 25m 30m' a
complete -W 'audio dpms rfkill xkb' status
complete -W 'audio dpms rfkill xkb' status-notify
complete -W 'n fg bg cur' color
complete -c cpath
complete -c csyspath
complete -c on
complete -c pacoc
complete -f paco

. /usr/share/bash-completion/completions/man
complete -F _man manl

_pacl() {
    COMPREPLY=()
    . /usr/share/bash-completion/completions/pacman
    _get_comp_words_by_ref cur prev
    _pacman_pkg Qq
}
complete -F _pacl pacl

[ "$(type -t wo)" = 'alias' ] \
    && complete -o default -o nospace -F _virtualenvs wo
