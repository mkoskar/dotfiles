# Source this file to get common aliases and functions.
# :Compatibility: POSIX / Bourne
#
# TODO: cleanup bashisms

set -o noclobber

# non-POSIX but supported by bash(1) and zsh(1) at least
set -o pipefail || true

ti_hi0=$'\e[1;37m'
ti_hi1=$'\e[1;32m'
ti_hi2=$'\e[1;31m'
ti_hi3=$'\e[1;33m'
ti_reset=$'\e[m'

CDPATH='.:..:~'
PS4=$'+\n$ti_hi1>$ti_reset [$(date +%T)] $ti_hi3$BASH_SOURCE:$LINENO$ti_reset\n$ti_hi1>$ti_reset $ti_hi0$BASH_COMMAND$ti_reset\n$ti_hi1>$ti_reset '
unset MAILCHECK

if [ "$BASH_VERSION" ]; then
    shopt -s expand_aliases
    unset BASH_ENV
fi


# ls
# ----------------------------------------

alias ls='ls --group-directories-first --color=auto'
alias l='ls -1A'
alias la='ll -A'
alias lc='lt -c'
alias lk='ll -Sr'
alias ll='ls -lh'
alias lr='ll -R'
alias lt='ll -tr'
alias lu='lt -u'
alias lx='ll -XB'


# man
# ----------------------------------------

alias man-less="MANPAGER='less -s' man"

alias man-1p='man -s 1p'
alias man-3p='man -s 3p'
alias man-posix='man -s 1p,2p,3p,4p,5p,6p,7p,8p,9p'

man-all() {
    pgx man -k . "$@"
}

alias man-all-1p='man-all -s 1p'
alias man-all-3p='man-all -s 3p'
alias man-all-posix='man-all -s 1p,2p,3p,4p,5p,6p,7p,8p,9p'


# ack / ag / grep
# ----------------------------------------

alias ack='ack --color'
alias ag='ag --color --color-path=36 --color-line-number=33 --color-match=41 --follow --nobreak --smart-case --noheading'
alias grep='LC_ALL=C grep --color=auto'
alias g='grep -n --color=always'
alias gi='g -i'
alias gr="g -r --exclude-dir='.svn' --exclude-dir='.git' --exclude='*.swp' --exclude='*~'"
alias gri='gr -i'


# python
# ----------------------------------------

alias py='python'
alias ipy='ipython'
alias q='deactivate'


# pyenv
# ----------------------------------------

if [ -d "$PYENV_ROOT" ]; then
    if [ "$BASH_VERSION" ]; then
        . "$PYENV_ROOT/completions/pyenv.bash"
    elif [ "$ZSH_VERSION" ]; then
        . "$PYENV_ROOT/completions/pyenv.zsh"
    fi
fi


# docker
# ----------------------------------------

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
    local target; target=${1:-$(docker ps -lq)}
    [ ! "$target" ] && return 2
    docker inspect --format '{{ .NetworkSettings.IPAddress }}' "$target"
}

dkrm() {
    confirm 'Remove ALL containers (with volumes). Continue?' n || return 0
    docker ps -aq | xargs -r docker rm -v -f
}

dkstop() {
    docker ps -aq | xargs -r docker stop
}


# java
# ----------------------------------------

alias java-info='java -XshowSettings:all -version'


# groovy
# ----------------------------------------

alias groovy-grape-verbose='groovy -Dgroovy.grape.report.downloads=true'


# maven
# ----------------------------------------

alias mvn-dependency-tree='mvn dependency:tree'
alias mvn-effective-pom='mvn help:effective-pom'
alias mvn-effective-settings='mvn help:effective-settings'

mvn-describe-plugin() {
    [ $# -eq 0 ] && return 2
    mvn help:describe -Dplugin="$1"
}

mvn-archetype-generate() {
    mvn archetype:generate -Dfilter="$1"
}


# gradle
# ----------------------------------------

alias gradle-tasks='gradle -q tasks --all'
alias gradle-dependencies='gradle -q dependencies'


# node
# ----------------------------------------

alias npmg='npm -g'


# pacman
# ----------------------------------------

# Finds what package provides file or directory
paco() {
    [ $# -eq 0 ] && return 2
    pacman -Qo -- "$@"
}

# Finds what package provides command
pacoc() {
    [ $# -eq 0 ] && return 2
    ptha "$1" | xargs -d '\n' -r pacman -Qo
}

# Files provided by package
pacl() {
    [ $# -eq 0 ] && return 2
    pacman -Qql -- "$1"
}

# Target's 'depends on'
pacd() {
    [ $# -eq 0 ] && return 2
    expac -l '\n' %D "$1"
}

# Target's 'provides'
pacp() {
    [ $# -eq 0 ] && return 2
    expac -l '\n' %P "$1"
}

# Target's 'required by' (what depends on target)
pacw() {
    [ $# -eq 0 ] && return 2
    expac -l '\n' %N "$1"
}

# Target's detailed info
paci() {
    [ $# -eq 0 ] && return 2
    pacman -Qii -- "$@"
}


# Other
# ----------------------------------------

alias ..='cd ..'
alias ...='cd ../..'
alias ....='cd ../../..'

alias acpi='acpi -V'
alias aunpack='aunpack -q'
alias cal='cal -3 -m'
alias callgrind='valgrind --tool=callgrind'
alias cower='cower --color=auto'
alias cp='cp -ai'
alias date0='date -Ins'
alias df='df -h'
alias dmesg='dmesg -HTx'
alias dstat='dstat -cglmnpry --tcp'
alias du='du -sh'
alias feh='feh -F'
alias fortune='fortune -c'
alias gpg-sandbox='gpg --homedir ~/.gnupg/sandbox'
alias info='info --vi-keys'
alias journal='journalctl -o short-precise -r -b'
alias llib='tree ~/.local/lib'
alias lsblk='lsblk -o NAME,KNAME,MAJ:MIN,ROTA,RM,RO,TYPE,SIZE,FSTYPE,MOUNTPOINT,MODEL'
alias lsdiff='lsdiff -s'
alias ltime='date +%T'
alias mpv-debug='command mpv --msg-level=all=debug'
alias mpv-verbose='command mpv --msg-level=all=v'
alias mutt-debug='mutt -d 2'
alias mv='mv -i'
alias od='od -Ax -tc -tx1 -v -w16'
alias odd='od -td1'
alias odo='od -to1'
alias odx='/usr/bin/od -Ax -tx2z -v -w16'
alias pac='pacman'
alias paccheck='paccheck --files --file-properties --backup --noextract --noupgrade'
alias pactree='pactree --color'
alias patch0='patch -Np0'
alias patch1='patch -Np1'
alias ping-mtu='ping -M do -s 2000'
alias psa='ps auxf'
alias pulse-streams='pacmd list-sink-inputs'
alias qiv='qiv -uLtiGfl --vikeys'
alias rax='rax2'
alias rm='rm -I --one-file-system'
alias sd='sudo systemctl'
alias sdu='systemctl --user'
alias se='sudoedit'
alias ss='ss -napstu'
alias stat="stat -c '%A %a %h %U %G %s %y %N'"
alias sudo0='sudo -K && sudo -k'
alias vgfull='valgrind --leak-check=full --show-reachable=yes'
alias watch='watch -n 1 -t -c'
alias wtc='curl -sL http://whatthecommit.com/index.txt'
alias youtube-dl-stdout='youtube-dl -o -'

# Simple alarm (defaults to 5 minutes)
a() {
    local d=${1:-5m}
    printf '%s ... alarm after %s\n' "$(\date -R)" "$d"
    sleep "${1:-5m}"
    echo 'Beep...'
    notify-send -u critical 'Beep...' "Time's up!"
    mpv --loop=10 --keep-open=no /usr/share/sounds/freedesktop/stereo/alarm-clock-elapsed.oga
}

base() { export BASEDIR=${1:-$PWD}; }
unbase() { unset BASEDIR; }

alias cd='__cd'
__cd() {
    [ $# -eq 0 ] && \cd "${BASEDIR:-${SHOME:-$HOME}}" || \cd "$@"
}

date() {
    [ $# -eq 0 ] && set -- -R
    command date "$@"
}

lsmod() {
    pgx command lsmod "$@"
}

lspci() {
    pgx command lspci -vv -nn "$@"
}

lsusb() {
    pgx command lsusb -v "$@"
}

tree() {
    set -- --dirsfirst -a -I '.git|.svn' --noreport -x "$@"
    if [ -t 1 ]; then
        pgx command tree -C "$@"
    else
        command tree "$@"
    fi
}

pstree() {
    pgx command pstree -ahglnpsSuU "$@"
}

mplayer() { command mplayer -really-quiet -msglevel all=1 "$@" 2>/dev/null; }
mpv() { command mpv --really-quiet --msg-level=all=error "$@" 2>/dev/null; }
smplayer() { command smplayer "$@" >/dev/null 2>&1; }
vlc() { command vlc "$@" 2>/dev/null; }

xrandr() {
    if [ $# -eq 0 -a -t 1 ]; then
        pgx command xrandr --properties --verbose
    else
        command xrandr "$@"
    fi
}

xserver-log() {
    local dispno; dispno=${1:-$(xserverq dispno)}
    [ "$dispno" ] || return 1
    $PAGER ~/.local/share/xorg/Xorg."$dispno".log
}

xsession-out() {
    local dispno; dispno=${1:-$(xserverq dispno)}
    [ "$dispno" ] || return 1
    $PAGER ~/.local/share/xorg/xsession."$dispno".out
}

on() {
    [ $# -eq 0 ] && return 2
    local p; p=$(pth "$1") || return 1
    shift
    eval "${*:-ls -la}" "$p"
}

i() {
    if [ $# -eq 0 ]; then
        shi
    elif [ "$ZSH_VERSION" ]; then
        type -af "$1"
    else
        type -a "$1"
    fi
}

v() {
    if [ $# -eq 0 ]; then
        declare -p | "$PAGER"
    else
        declare -p -- "$1"
    fi
}

fn() {
    if [ $# -eq 0 ]; then
        declare -f | "$PAGER"
    else
        declare -f -- "$1" || return 1
        [ "$BASH_VERSION" ] && \
            (shopt -s extdebug; printf '# '; declare -F -- "$1")
    fi
}

setpid() {
    PID=
    if [ "$BASH_VERSION" ]; then
        PID=$BASHPID
    elif [ "$ZSH_VERSION" ]; then
        PID=${sysparams[pid]}
    fi
    [ "$PID" ] || return 1
}

shi() {
    setpid
    cmdline
    printf '$$: %s\n' "$$"
    printf 'PID: %s\n' "$PID"
    printf 'PPID: %s\n' "$PPID"
    printf 'SHLVL: %s\n' "$SHLVL"
    printf 'SUBSHELL: %s\n' "${BASH_SUBSHELL:-$ZSH_SUBSHELL}"
    printf 'VERSION: %s\n' "${BASH_VERSION:-$ZSH_VERSION}"
}

optset() {
    if [ $# -eq 0 ]; then
        set +o
    else
        [ "${#1}" -gt 1 ] && return 2
        case $- in *$1*) ;; *) return 1 ;; esac
    fi
}

debug() {
    shopt -s extdebug
    set -o errtrace
    set -o functrace
    trap stacktrace ERR
}

# TODO: still some unbound variables?
stacktrace() {
    local retstat=$? ti_header=$ti_hi1
    [ "$retstat" -gt 0 ] && ti_header=$ti_hi2
    echo "$ti_header> Traceback ($retstat):$ti_reset"
    shopt -q extdebug || echo '> WARN: extdebug not set!'
    for ((i=1, argvo=0; i<${#FUNCNAME[@]}; i++, argvo+=argc)); do
        argc=${BASH_ARGC[$i]}
        printf "$ti_hi3%s:%d$ti_reset $ti_hi0%s$ti_reset" \
            "${BASH_SOURCE[$((i+1))]}" \
            "${BASH_LINENO[$i]}" \
            "${FUNCNAME[$i]}"
        for ((j=argc-1; j>=0; j--)); do
            printf ' %q' "${BASH_ARGV[$((argvo+j))]}"
        done
        echo
    done
}

ifs() {
    printf '%s' "$IFS" | \od -An -ta -tx1
}

ifs0() {
    IFS=$' \t\n'
}

lsof() {
    setpid
    command lsof -p "${1:-$PID}"
}

reexec() {
    exec $(cmdline)
}

reload() {
    . ~/.profile
    if [ "$BASH_VERSION" ]; then
        . ~/.bashrc
    elif [ "$ZSH_VERSION" ]; then
        . ~/.zshrc
    else
        . ~/bin/shx.sh
    fi
}


# Bourne-like shell only
# ----------------------------------------

eval 'function __bourne_test { true; }' 2>/dev/null || return
unset __bourne_test

# virtualenvwrapper
if [ -e /usr/bin/virtualenvwrapper.sh ]; then
    . /usr/bin/virtualenvwrapper_lazy.sh

    alias mkvirtualenv2="mkvirtualenv -p '$(pth python2)'"
    alias mkvirtualenv3="mkvirtualenv -p '$(pth python3)'"
    alias wo='workon'

    mkvirtualenv-pyenv() {
        [ $# -eq 0 ] && return 2
        local ver=$1
        shift
        mkvirtualenv -p "$PYENV_ROOT/versions/$ver/bin/python" "$@"
    }
fi
