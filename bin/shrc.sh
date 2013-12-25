# Source this file to initialize shell.

[ -z "$INTERACTIVE" ] && return

[[ "$TERM" == 'screen' ]] && export TERM='screen-256color'

export GPG_TTY=$(tty)

stty -ixon
eval $(dircolors -b)

alias ls='ls -h --group-directories-first --color=auto'
alias l='ls -la'
alias grep='grep --color=auto'
alias igrep='grep -i'
alias g="grep --exclude-dir='.svn' --exclude-dir='.git' --exclude='*.swp' --exclude='*~'"
alias stat="stat -c '%A %a %h %U %G %s %y %n'"
alias info='info --vi-keys'
alias nw='tmux neww'

# gpg
alias gpgsandbox='gpg --homedir ~/.gnupg/sandbox'

# virtualenvwrapper
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
