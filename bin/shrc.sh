# Source this file to initialize shell.
# :Compatibility: POSIX / Bourne

# during the X server startup TERMINFO is set, but we don't want it
unset TERMINFO

tty="$(tty)" && export GPG_TTY="$tty"
unset tty

# continue only in case of interactive shell
# ------------------------------------------
case $- in *i*) ;; *) return ;; esac

. ~/bin/term.sh
tput init

# turn off flow control
stty -ixon -ixoff

# prevent overwriting an existing file when doing redirects
set -o noclobber

alias ls='ls -h --group-directories-first --color=auto'
alias l='ls -la'
alias rm='rm -I --one-file-system'

alias grep='LC_ALL=C grep --color=auto'
alias g="grep"
alias gi="g -i"
alias gr="g -r --exclude-dir='.svn' --exclude-dir='.git' --exclude='*.swp' --exclude='*~'"
alias gri="gr -i"

alias feh='feh -F'
alias gpgsandbox='gpg --homedir ~/.gnupg/sandbox'
alias info='info --vi-keys'
alias manl="MANPAGER='less -s' man"
alias nw='tmux neww'
alias qiv='qiv -uLtiGfl --vikeys'
alias stat="stat -c '%A %a %h %U %G %s %y %N'"
alias sxiv='sxiv -f'

# python
alias py='python'
alias ipy='ipython'
alias pipinst='pip install --download-cache=~/.pip-cache'

# mplayer
alias play='mplayer -really-quiet'
alias playcd='play cdda://'
alias playdvd='play -mouse-movements dvdnav://'
alias playvcd='play vcd://2'

# simple alarm (defaults to 5 minutes)
a() {
    local dur
    dur="${1:-5m}"
    printf '%s ... alarm after %s\n' "$(date)" "$dur"
    sleep "${1:-5m}"
    echo 'Beep...'
    play -loop 0 /usr/share/sounds/freedesktop/stereo/alarm-clock-elapsed.oga
}

# returns command executable on current PATH
cpath() {
    local cmd
    cmd="$(command -v "$1")" || return 1
    [ -z "${cmd##/*}" ] && echo "$cmd"
}

# returns command executable on system PATH
csyspath() {
    (PATH='/bin:/usr/bin' cpath "$@")
}

# lists open file descriptors of a process with passed PID (defaults to '$$')
lsfd() {
    ls -l "/proc/${1:-$$}/fd"
}

# finds '$1' on PATH and append it as an argument to the rest of cmdline
# (defaults to 'ls -la')
on() {
    local path
    path="$(cpath "$1")" || return 1
    shift
    eval "${*:-ls -la}" "$path"
}

# finds what package provides passed file or directory
paco() {
    local path
    path="$(readlink -f "$1")"
    [ -e "$path" ] && pacman -Qo "$path"
}

# finds what package provides passed command
pacoc() {
    local path
    path="$(cpath "$1")"
    [ -n "$path" ] && pacman -Qo "$path"
    path="$(csyspath "$1")"
    [ -n "$path" ] && pacman -Qo "$path"
}

# lists files provided by passed package
pacl() {
    pacman -Ql "$1" | pg
}

# continue only in case of Bourne shell
# -------------------------------------
eval 'function _bourne_test { true; }' 2>/dev/null || return
unset _bourne_test

# virtualenvwrapper
if [ -f /usr/bin/virtualenvwrapper.sh ]; then
    . /usr/bin/virtualenvwrapper.sh
    alias mkvirtualenv2="mkvirtualenv -p $(cpath python2)"
    alias mkvirtualenv3="mkvirtualenv -p $(cpath python3)"
    alias wo='workon'
fi
