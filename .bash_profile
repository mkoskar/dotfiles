# ~/.bash_profile
# - executed by bash(1) for login shells

[[ $SHRC_DEBUG ]] && echo '~/.bash_profile' >&2

[[ -e ~/.profile ]] && . ~/.profile
[[ -e ~/.bashrc ]] && . ~/.bashrc

return 0
