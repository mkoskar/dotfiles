# Source this file to initialize shell.
# :Compatibility: POSIX / Bourne

[ -e ~/bin/term.sh ] && . ~/bin/term.sh

# continue only in case of interactive shell
# ------------------------------------------
case $- in *i*) ;; *) return ;; esac

# prevent overwriting an existing file when doing redirects
set -o noclobber

# ls
alias ls='ls --group-directories-first --color=auto'
alias l='ls -1A'
alias la='ll -A'
alias lc='lt -c'
alias lk='ll -Sr'
alias ll='ls -lh'
alias lm='la --color=always | "$PAGER"'
alias lr='ll -R'
alias lt='ll -tr'
alias lu='lt -u'
alias lx='ll -XB'

# grep
alias grep='LC_ALL=C grep --color=auto'
alias g='grep'
alias gi='g -i'
alias gr="g -R --exclude-dir='.svn' --exclude-dir='.git' --exclude='*.swp' --exclude='*~'"
alias gri='gr -i'

# python
alias py='python'
alias ipy='ipython'
alias pipinst='pip install --download-cache=~/.pip-cache'

# mplayer
alias play='mplayer -really-quiet'
alias playcd='play cdda://'
alias playdvd='play -mouse-movements dvdnav://'
alias playvcd='play vcd://2'

# other
alias ..='cd ..'
alias ...='cd ../..'
alias ....='cd ../../..'
alias acpi='acpi -V'
alias callgrind='valgrind --tool=callgrind'
alias cower='cower --color=auto'
alias feh='feh -F'
alias gpgsandbox='gpg --homedir ~/.gnupg/sandbox'
alias info='info --vi-keys'
alias lsdiff='lsdiff -s'
alias manl="MANPAGER='less -s' man"
alias ntpdq='sudo ntpd -q && sudo hwclock -w'
alias pac='pacman'
alias qiv='qiv -uLtiGfl --vikeys'
alias rm='rm -I --one-file-system'
alias sd='systemctl'
alias sdu='systemctl --user'
alias se='sudoedit'
alias stat="stat -c '%A %a %h %U %G %s %y %N'"
alias vgfull='valgrind --leak-check=full --show-reachable=yes'
alias wtc='curl --silent http://whatthecommit.com/index.txt'

# simple alarm (defaults to 5 minutes)
a() {
    local d="${1:-5m}"
    printf '%s ... alarm after %s\n' "$(date)" "$d"
    sleep "${1:-5m}"
    echo 'Beep...'
    play -loop 0 /usr/share/sounds/freedesktop/stereo/alarm-clock-elapsed.oga
}

# returns command executable on current PATH
pth() {
    local c
    c="$(command -v "$1")" || return 1
    [ -z "${c##/*}" ] && echo "$c"
}

# returns command executable on system PATH
spth() {
    (PATH='/bin:/usr/bin' pth "$@")
}

# lists open file descriptors of a process with passed PID (defaults to '$$')
lsfd() {
    ls -l "/proc/${1:-$$}/fd"
}

# finds '$1' on PATH and append it as an argument to the rest of cmdline
# (defaults to 'ls -la')
on() {
    local p
    p="$(pth "$1")" || return 1
    shift
    eval "${*:-ls -la}" "$p"
}

# finds what package provides file or directory
paco() {
    local p="$(readlink -f "$1")"
    [ -e "$p" ] && pacman -Qo "$p"
}

# finds what package provides command
pacoc() {
    local p
    p="$(pth "$1")" && pacman -Qo "$p"
    p="$(spth "$1")" && pacman -Qo "$p"
}

# files provided by package
pacl() {
    pacman -Qql "$1" | pg
}

# target 'depends on'
pacd() {
    expac -l '\n' %D "$1"
}

# target 'provides'
pacp() {
    expac -l '\n' %P "$1"
}

# target 'required by' (what depends on target)
pacw() {
    expac -l '\n' %N "$1"
}

# target detailed info
paci() {
    local p="$(expac %n "$1")"
    [ -z "$p" ] && return 1
    pacman -Qii "$p"
}

# continue only in case of Bourne-like shell
# ------------------------------------------
eval 'function _bourne_test { true; }' 2>/dev/null || return
unset _bourne_test

# virtualenvwrapper
if [ -e /usr/bin/virtualenvwrapper.sh ]; then
    . /usr/bin/virtualenvwrapper_lazy.sh
    alias mkvirtualenv2="mkvirtualenv -p $(pth python2)"
    alias mkvirtualenv3="mkvirtualenv -p $(pth python3)"
    alias wo='workon'
    if [ -n "$BASH" ]; then
        _virtualenvwrapper_load() {
            virtualenvwrapper_load
            complete -o default -o nospace -F _virtualenvs wo
        }
        complete -o nospace -F _virtualenvwrapper_load wo
    fi
fi
