# Source this file to get common aliases and functions.
# :Compatibility: POSIX / Bourne

set -o noclobber

if [ -n "$BASH" ]; then
    shopt -s expand_aliases
    unset BASH_ENV
fi

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

# ack / grep
alias ac='ack --color'
alias grep='LC_ALL=C grep --color=auto'
alias g='grep -n --color=always'
alias gi='g -i'
alias gr="g -R --exclude-dir='.svn' --exclude-dir='.git' --exclude='*.swp' --exclude='*~'"
alias gri='gr -i'

# python
alias py='python'
alias ipy='ipython'
alias pip-no-require-venv='PIP_REQUIRE_VIRTUALENV= '
alias pipinst='pip install --download-cache=~/.pip-cache'

# mplayer
play() { mplayer -really-quiet "$@" 2>/dev/null; }
alias playcd='play cdda://'
alias playdvd='play -mouse-movements dvdnav://'
alias playvcd='play vcd://2'

# docker
alias dk='docker'
alias dkb='docker build'
alias dkc='docker ps'
alias dkca='docker ps -a'
alias dkcl='docker ps -l -q'
alias dke='docker exec -i -t'
alias dki='docker images'
alias dkia='docker images -a'
alias dkr='docker run -e TERM="${TERM%%-bsdel}" -P'
alias dkrd='docker run -e TERM="${TERM%%-bsdel}" -d -P'
alias dkri='docker run -e TERM="${TERM%%-bsdel}" -i -t -P'

dkip() {
    local target=${1:-$(docker ps -lq)}
    [ -z "$target" ] && return 2
    docker inspect --format '{{ .NetworkSettings.IPAddress }}' "$target"
}

dkrm() {
    confirm 'About to remove ALL containers. Continue?' n || return 0
    local ids
    ids=($(docker ps -aq))
    [ ${#ids[@]} -gt 0 ] && docker rm -f "${ids[@]}" || true
}

dkstop() {
    local ids
    ids=($(docker ps -aq))
    [ ${#ids[@]} -gt 0 ] && docker stop "${ids[@]}" || true
}

# other
alias ..='cd ..'
alias ...='cd ../..'
alias ....='cd ../../..'
alias acpi='acpi -V'
alias callgrind='valgrind --tool=callgrind'
alias cower='cower --color=auto'
alias df='df -h'
alias du='du -sh'
alias feh='feh -F'
alias gpg-sandbox='gpg --homedir ~/.gnupg/sandbox'
alias info='info --vi-keys'
alias loclib='tree ~/.local/lib'
alias lsdiff='lsdiff -s'
alias manl="MANPAGER='less -s' man"
alias ntpdq='sudo ntpd -q && sudo hwclock -w'
alias pac='pacman'
alias pactree='pactree --color'
alias psa='ps auxf'
alias qiv='qiv -uLtiGfl --vikeys'
alias rm='rm -I --one-file-system'
alias sd='sudo systemctl'
alias sdu='systemctl --user'
alias se='sudoedit'
alias stat="stat -c '%A %a %h %U %G %s %y %N'"
alias vgfull='valgrind --leak-check=full --show-reachable=yes'
alias watch='watch -n1 -t -c'
alias wtc='curl --silent http://whatthecommit.com/index.txt'

# simple alarm (defaults to 5 minutes)
a() {
    local d=${1:-5m}
    printf '%s ... alarm after %s\n' "$(date)" "$d"
    sleep "${1:-5m}"
    echo 'Beep...'
    play -loop 0 /usr/share/sounds/freedesktop/stereo/alarm-clock-elapsed.oga
}

alias cd='__cd'
__cd() {
    [ $# -eq 0 ] && \cd "${BASEDIR:-$HOME}" || \cd "$@"
}

# returns command executable on current PATH
pth() {
    local c
    c=$(command -v "$1") || return 1
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
    p=$(pth "$1") || return 1
    shift
    eval "${*:-ls -la}" "$p"
}

# finds what package provides file or directory
paco() {
    local p=$(realpath "$1")
    [ -e "$p" ] && pacman -Qo "$p"
}

# finds what package provides command
pacoc() {
    local p
    p=$(pth "$1") && pacman -Qo "$p"
    p=$(spth "$1") && pacman -Qo "$p"
}

# files provided by package
pacl() {
    pacman -Qql "$1"
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
    local p=$(expac %n "$1")
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
    alias mkvirtualenv2="mkvirtualenv -p '$(pth python2)'"
    alias mkvirtualenv3="mkvirtualenv -p '$(pth python3)'"
    alias wo='workon'
    if [ -n "$BASH" ]; then
        _virtualenvwrapper_load() {
            virtualenvwrapper_load
            complete -o default -o nospace -F _virtualenvs wo
        }
        complete -o nospace -F _virtualenvwrapper_load wo
    fi
fi
