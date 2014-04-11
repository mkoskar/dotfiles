# ~/.bashrc
# - executed by bash(1) for non-login shells

[[ $- == *i* ]] && INTERACTIVE='yes'

[ -f ~/bin/shrc.sh ] && source ~/bin/shrc.sh

[ -z "$INTERACTIVE" ] && return

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

PS1='\u@\h:\!:\#:\W\$ '
PS1='\#:\W\$ '
PROMPT_DIRTRIM=3
CDPATH='.:..:~'
unset MAILCHECK
