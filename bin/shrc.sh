# Source this file to initialize shell.

# during the X server startup TERMINFO is set, but we don't want it
unset TERMINFO

TTY=$(tty) && export GPG_TTY="$TTY"

[ -z "$INTERACTIVE" ] && return

source ~/bin/term.sh
tput init

stty -ixon

eval $(TERM=xterm dircolors -b)

alias ls='ls -h --group-directories-first --color=auto'
alias l='ls -la'
alias grep='grep --color=auto'
alias igrep='grep -i'
alias g="grep --exclude-dir='.svn' --exclude-dir='.git' --exclude='*.swp' --exclude='*~'"
alias stat="stat -c '%A %a %h %U %G %s %y %n'"
alias info='info --vi-keys'
alias nw='tmux neww'
alias gpgsandbox='gpg --homedir ~/.gnupg/sandbox'

# virtualenv
export VIRTUALENVWRAPPER_PYTHON="$(which python2)"
source ~/.local/bin/virtualenvwrapper.sh
alias mkvirtualenv2="mkvirtualenv -p $(which python2)"
alias mkvirtualenv3="mkvirtualenv -p $(which python3)"
alias pipinst='pip install --download-cache=~/.pip-cache'
alias wo='workon'
complete -o default -o nospace -F _virtualenvs wo

# python
alias py='python'
alias ipy='ipython'

# mplayer
alias play='mplayer -really-quiet'
alias playcd='play cdda://'
alias playdvd='play -mouse-movements dvdnav://'
alias playvcd='play vcd://2'

radio() { 
    local PS3='Select a station: '
    local stations=(
        'BBC 1                      http://www.bbc.co.uk/radio/listen/live/r1.asx'
        'BBC 1Xtra                  http://www.bbc.co.uk/radio/listen/live/r1x.asx'
        'BBC 2                      http://www.bbc.co.uk/radio/listen/live/r2.asx'
        'BBC 3                      http://www.bbc.co.uk/radio/listen/live/r3.asx'
        'BBC 4                      http://www.bbc.co.uk/radio/listen/live/r4.asx'
        'BBC 4Xtra                  http://www.bbc.co.uk/radio/listen/live/r4x.asx'
        'BBC 5 Live                 http://www.bbc.co.uk/radio/listen/live/r5l.asx'
        'BBC 5 Live Sports Extra    http://www.bbc.co.uk/radio/listen/live/r5lsp.asx'
        'BBC 6 Music                http://www.bbc.co.uk/radio/listen/live/r6.asx'
        'BBC Asian Network          http://www.bbc.co.uk/radio/listen/live/ran.asx'
        'BBC World Service          http://www.bbc.co.uk/worldservice/meta/live/nb/eieuk_au_nb.asx'
        'BBC London                 http://www.bbc.co.uk/radio/listen/live/bbclondon.asx'
    )
    local sel
    select sel in "${stations[@]}"; do
        break
    done
    sel=($sel)
    play -playlist "${sel[@]: -1}"
}

a() {
    sleep ${1:-5m} && play -loop 0 /usr/share/sounds/freedesktop/stereo/alarm-clock-elapsed.oga
}
