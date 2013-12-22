# ~/.bashrc
# - executed by bash(1) for non-login shells
# - this is also included in ~/.profile

export LANG='en_US.UTF-8'
export LC_COLLATE='C'
export EDITOR='vim'
export BROWSER='firefox'

export JDK_HOME="$JAVA_HOME"
export _JAVA_OPTIONS='-Dawt.useSystemAAFontSettings=on'
export _JAVA_AWT_WM_NONREPARENTING='1'
export ORACLE_HOME='/opt/instantclient'
export SAL_USE_VCLPLUGIN='gen'

alias ls='ls -h --group-directories-first --color=auto'
alias l='ls -la'
alias grep='grep --color=auto'
alias igrep='grep -i'
alias g="grep --exclude-dir='.svn' --exclude-dir='.git' --exclude='*.swp' --exclude='*~'"
alias stat="stat -c '%A %a %h %U %G %s %y %n'"
alias info='info --vi-keys'
alias nw='tmux neww'

# check for an interactive session
[ -z "$PS1" ] && return

[[ "$TERM" == 'screen' ]] && export TERM='screen-256color'

stty -ixon
export GPG_TTY=$(tty)

eval `dircolors -b`

PS1='\u@\h:\!:\#:\W\$ '
PROMPT_DIRTRIM=3
CDPATH='.:..:~'
unset MAILCHECK

shopt -s autocd checkjobs cmdhist dotglob histappend \
         histreedit histverify lithist no_empty_cmd_completion

bind -m vi-insert   '"\C-e": shell-expand-line'
bind -m vi-command  '"\C-e": shell-expand-line'
bind -m vi-insert   '"\ee":  history-and-alias-expand-line'
bind -m vi-insert   '"\eE":  history-and-alias-expand-line'
bind -m vi-command  '"\ee":  history-and-alias-expand-line'
bind -m vi-command  '"\eE":  history-and-alias-expand-line'
bind -m vi-insert   '"\ei":  complete-filename'
bind -m vi-insert   '"\eI":  complete-filename'
bind -m vi-insert   '"\e\t": dynamic-complete-history'
bind -m vi-insert   '"\eq":  dabbrev-expand'

export HISTCONTROL='erasedups'
export HISTFILESIZE='5000'
export HISTSIZE='500'
export HISTIGNORE='exit'

export LESS='-MRS#3'
export LESSHISTFILE='-'

# gpg
alias gpgsandbox='gpg --homedir "${HOME}/.gnupg/sandbox"'

# virtualenvwrapper
export VIRTUALENVWRAPPER_PYTHON="$(which python2)"
source "$HOME/.local/bin/virtualenvwrapper.sh"
alias mkvirtualenv2="mkvirtualenv -p $(which python2)"
alias mkvirtualenv3="mkvirtualenv -p $(which python3)"
alias pipinst='pip install --download-cache="$HOME/.pip-cache"'
alias wo='workon'
complete -o default -o nospace -F _virtualenvs wo

# python
alias py='python'
alias ipy='ipython'

# mplayer
alias play='mplayer -msgcolor -msgmodule'
alias playcd='mplayer cdda://'
alias playdvd='mplayer -mouse-movements dvdnav://'
alias playvcd='mplayer vcd://2'

# BBC radio
bbcradio() { 
    local s PS3="Select a station: "
    select s in 1 1x 2 3 4 4x 5l 5lsp 6 "Asian Network an" "Nations & Local lcl"; do
        break
    done
    s=($s)
    play -playlist "http://www.bbc.co.uk/radio/listen/live/r${s[@]: -1}.asx"
}

alias bbclondon='play -playlist http://www.bbc.co.uk/radio/listen/live/bbclondon.asx'
alias bbcworld='play -playlist http://www.bbc.co.uk/worldservice/meta/tx/nb/live/eneuk.asx'
