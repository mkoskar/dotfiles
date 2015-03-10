# Source this file to get common aliases and functions.
# :Compatibility: POSIX / Bourne

set -o noclobber

unset MAILCHECK

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

# ack / ag / grep
alias ack='ack --color'
alias ag='ag --color --color-path=36 --color-line-number=33 --color-match=41 --follow --nobreak --smart-case --noheading'
alias grep='LC_ALL=C grep --color=auto'
alias g='grep -n --color=always'
alias gi='g -i'
alias gr="g -r --exclude-dir='.svn' --exclude-dir='.git' --exclude='*.swp' --exclude='*~'"
alias gri='gr -i'

# python
alias py='python'
alias ipy='ipython'

# docker
alias dk='docker'
alias dkb='docker build'
alias dkc='docker ps'
alias dkca='docker ps -a'
alias dkcl='docker ps -l -q'
alias dke='docker exec -i -t'
alias dki='docker images'
alias dkia='docker images -a'
alias dkr='docker run -P'
alias dkrd='docker run -d -P'
alias dkri='docker run -i -t -P'

dkip() {
    local target=${1:-$(docker ps -lq)}
    [ -z "$target" ] && return 2
    docker inspect --format '{{ .NetworkSettings.IPAddress }}' "$target"
}

dkrm() {
    confirm 'Remove ALL containers (with volumes). Continue?' n || return 0
    local ids
    ids=($(docker ps -aq))
    [ ${#ids[@]} -eq 0 ] || docker rm -v -f "${ids[@]}"
}

dkstop() {
    local ids
    ids=($(docker ps -aq))
    [ ${#ids[@]} -eq 0 ] || docker stop "${ids[@]}"
}

# maven
alias mvn-effective-pom='mvn help:effective-pom'
alias mvn-effective-settings='mvn help:effective-settings'
alias mvn-dependency-tree='mvn dependency:tree'

mvn-describe-plugin() {
    [ $# -eq 0 ] && return 2
    mvn help:describe -Dplugin="$1"
}

# gradle
alias gradle-tasks='gradle -q tasks --all'
alias gradle-dependencies='gradle -q dependencies'

# node
alias npmg='npm -g'

# other
alias ..='cd ..'
alias ...='cd ../..'
alias ....='cd ../../..'
alias acpi='acpi -V'
alias aunpack='aunpack -q'
alias cal='cal -3 -m'
alias callgrind='valgrind --tool=callgrind'
alias cower='cower --color=auto'
alias cp='cp -ai'
alias date='date -R'
alias df='df -h'
alias dstat='dstat -cglmnpry --tcp'
alias du='du -sh'
alias feh='feh -F'
alias fortune='fortune -c'
alias gpg-sandbox='gpg --homedir ~/.gnupg/sandbox'
alias info='info --vi-keys'
alias journal='journalctl -o short-precise -r -b'
alias loclib='tree ~/.local/lib'
alias lsblk='lsblk -o NAME,KNAME,MAJ:MIN,ROTA,RM,RO,TYPE,SIZE,FSTYPE,MOUNTPOINT,MODEL'
alias lsdiff='lsdiff -s'
alias manl="MANPAGER='less -s' man"
alias ntpdq='sudo ntpd -q && sudo hwclock -w'
alias pac='pacman'
alias paccheck='paccheck --files --file-properties --backup --noextract --noupgrade'
alias pactree='pactree --color'
alias patch0='patch -Np0'
alias patch1='patch -Np1'
alias psa='ps auxf'
alias qiv='qiv -uLtiGfl --vikeys'
alias rm='rm -I --one-file-system'
alias sd='sudo systemctl'
alias sdu='systemctl --user'
alias se='sudoedit'
alias ss='ss -napstu'
alias stat="stat -c '%A %a %h %U %G %s %y %N'"
alias type='type -a'
alias vgfull='valgrind --leak-check=full --show-reachable=yes'
alias watch='watch -n1 -t -c'
alias wtc='curl --silent http://whatthecommit.com/index.txt'

# simple alarm (defaults to 5 minutes)
a() {
    local d=${1:-5m}
    printf '%s ... alarm after %s\n' "$(\date -R)" "$d"
    sleep "${1:-5m}"
    echo 'Beep...'
    notify-send -u critical 'Beep...' "Time's up!"
    mpv --loop inf /usr/share/sounds/freedesktop/stereo/alarm-clock-elapsed.oga
}

base() {
    export BASEDIR=${1:-$PWD}
}

unbase() {
    unset BASEDIR
}

alias cd='__cd'
__cd() {
    [ $# -eq 0 ] && \cd "${BASEDIR:-${SHOME:-$HOME}}" || \cd "$@"
}

mplayer() { /usr/bin/mplayer -really-quiet -msglevel all=1 "$@" 2>/dev/null; }
mpv() { /usr/bin/mpv --really-quiet --msg-level=all=error "$@" 2>/dev/null; }
smplayer() { /usr/bin/smplayer "$@" >/dev/null 2>&1; }
vlc() { /usr/bin/vlc "$@" 2>/dev/null; }

# returns command executable on current PATH
pth() {
    [ $# -eq 0 ] && return 2
    local c
    c=$(command -v "$1") || return 1
    [ -z "${c##/*}" ] && echo "$c" || {
        type -a "$1" >&2
        return 1
    }
}

# returns command executable on system PATH
spth() {
    (PATH='/usr/bin' pth "$@")
}

# lists open file descriptors of a process with passed PID (defaults to '$$')
lsfd() {
    ls -l "/proc/${1:-$$}/fd"
}

# finds '$1' on PATH and append it as an argument to the rest of cmdline
# (defaults to 'ls -la')
on() {
    [ $# -eq 0 ] && return 2
    local p
    p=$(pth "$1") || return 1
    shift
    eval "${*:-ls -la}" "$p"
}

# finds what package provides file or directory
paco() {
    [ $# -eq 0 ] && return 2
    local p=$(realpath -s "$1")
    pacman -Qo "$p"
    [ -L "$p" ] && pacman -Qo "$(realpath "$1")"
}

# finds what package provides command
pacoc() {
    [ $# -eq 0 ] && return 2
    local p
    p=$(pth "$1") && pacman -Qo "$p"
    p=$(spth "$1") && pacman -Qo "$p"
}

# files provided by package
pacl() {
    [ $# -eq 0 ] && return 2
    pacman -Qql "$1"
}

# target 'depends on'
pacd() {
    [ $# -eq 0 ] && return 2
    expac -l '\n' %D "$1"
}

# target 'provides'
pacp() {
    [ $# -eq 0 ] && return 2
    expac -l '\n' %P "$1"
}

# target 'required by' (what depends on target)
pacw() {
    [ $# -eq 0 ] && return 2
    expac -l '\n' %N "$1"
}

# target detailed info
paci() {
    [ $# -eq 0 ] && return 2
    local p=$(expac %n "$1")
    [ -z "$p" ] && return 1
    pacman -Qii "$p"
}

# returns shell's name and its version
shi() {
    if [ -n "$BASH_VERSION" ]; then
        echo "bash $BASH_VERSION"
    elif [ -n "$ZSH_VERSION" ]; then
        echo "zsh $ZSH_VERSION"
    else
        echo 'unknown'
    fi
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
