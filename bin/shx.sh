# Source this file to get common aliases and functions.
# :Compatibility: POSIX

[ "$SHRC_DEBUG" ] && echo '~/bin/shx.sh' >&2

if [ "$BASH_VERSION" ]; then
    shopt -s expand_aliases
    unset BASH_ENV
fi

SHNAME=$(cmdline -a0 $$)
SHNAME=${SHNAME##*/}
SHNAME=${SHNAME#-}
SHNAME=${SHNAME#r}

case $SHNAME in
    bash) shopt -qo posix && SHNAME='sh' ;;
esac

set -o noclobber

case $SHNAME in bash | zsh | mksh)
    set -o pipefail
esac


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
    # shellcheck disable=SC2033
    docker ps -aq | xargs -r docker rm -v -f
}

dkstop() {
    docker ps -aq | xargs -r docker stop
}


# grep / ack / ag / pt / rg
# ----------------------------------------

alias grep='LC_ALL=C grep --color=auto'
alias g='grep -n --color=always'
alias gi='g -i'
alias gr="g -r --exclude-dir='.svn' --exclude-dir='.git' --exclude='*.swp' --exclude='*~'"
alias gri='gr -i'
alias ack='ack --color'
alias ag='ag --color --color-path=36 --color-line-number=33 --color-match=41 --nobreak --smart-case --noheading'


# java / groovy / maven / gradle
# ----------------------------------------

alias gradle-dependencies='gradle -q dependencies'
alias gradle-tasks='gradle -q tasks --all'
alias groovy-grape-verbose='groovy -Dgroovy.grape.report.downloads=true'
alias java-info='java -XshowSettings:all -version'
alias mvn-dependency-tree='mvn dependency:tree'
alias mvn-effective-pom='mvn help:effective-pom'
alias mvn-effective-settings='mvn help:effective-settings'

mvn_archetype_generate() {
    mvn archetype:generate -Dfilter="$1"
}
alias mvn-archetype-generate='mvn_archetype_generate'

mvn_describe_plugin() {
    [ $# -eq 0 ] && return 2
    mvn help:describe -Dplugin="$1"
}
alias mvn-describe-plugin='mvn_describe_plugin'


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

man_all() {
    pgx man -k . "$@"
}
alias man-all='man_all'

alias man-all-1p='man-all -s 1p'
alias man-all-3p='man-all -s 3p'
alias man-all-posix='man-all -s 1p,2p,3p,4p,5p,6p,7p,8p,9p'


# pacman
# ----------------------------------------

alias pac='pacman'
alias pactree='pactree --color'

# Target's detailed info
paci() {
    [ $# -eq 0 ] && return 2
    pacman -Qii -- "$@" || pacman -Sii -- "$@" || cower -i -- "$@"
} 2>/dev/null

# Finds what package provides file or directory
paco() {
    [ $# -eq 0 ] && return 2
    pacman -Qo -- "$@"
}

# Finds what package provides command
pacoc() {
    [ $# -eq 0 ] && return 2
    pth -a "$1" | xargs -d '\n' -r pacman -Qo
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
    if [ $# -eq 0 ]; then
        expac -l ' ' '%n %P'
    else
        expac -l '\n' %P "$1"
    fi
}

pacs() {
    [ $# -eq 0 ] && return 2
    pacsearch "$1"
    cower --color=auto -s "$1"
}

# Target's 'required by' (what depends on target)
pacw() {
    [ $# -eq 0 ] && return 2
    expac -l '\n' %N "$1"
}

paccheck() {
    command paccheck --quiet --file-properties --sha256sum \
        --backup --noextract --noupgrade "$@"
}


# python
# ----------------------------------------

alias py='python'
alias ipy='ipython'
alias q='deactivate'


# Other
# ----------------------------------------

case $SHNAME in mksh) ;; *)
    alias ..='cd ..'
    alias ...='cd ../..'
    alias ....='cd ../../..'
esac

alias acpi='acpi -V'
alias an='asciinema'
alias aunpack='aunpack -q'
alias c='calc'
alias cal='cal -m -w -3'
alias callgrind='valgrind --tool=callgrind'
alias cower='cower --color=auto'
alias cp='cp -ai --reflink=auto'
alias date0='date -Ins'
alias dconfa='dconf dump /'
alias dd='dd status=progress'
alias df0='\df -h'
alias df='df -h -x tmpfs -x devtmpfs'
alias dirs='dirs -v'
alias dmesg='dmesg -HTx'
alias dstat='dstat -cglmnpry --tcp'
alias du='du -hx'
alias feh='feh -F'
alias fortune='fortune -c'
alias free='free -h'
alias gconfa='gconftool-2 -R /'
alias gpg-sandbox='gpg --homedir ~/.gnupg/sandbox'
alias grepcat='grep --exclude-dir=\* .'
alias gsettingsa='gsettings list-recursively'
alias headcat='head -vn-0'
alias info='info --vi-keys'
alias infocmp='infocmp -a -1'
alias journal-vaccum='journalctl --vacuum-size=100M --vacuum-files=1'
alias journal='journalctl -o short-precise -r -b'
alias llib='tree ~/.local/lib'
alias lsblk='lsblk -o NAME,KNAME,MAJ:MIN,ROTA,RM,RO,TYPE,SIZE,FSTYPE,MOUNTPOINT,MODEL'
alias lsdiff='lsdiff -s'
alias ltime='date +%T'
alias mnt='findmnt'
alias moon='curl -sSLf http://wttr.in/moon | head -n-4'
alias mount-loop='mount -o loop'
alias mpv-debug='mpv --msg-level=all=debug'
alias mpv-verbose='mpv --msg-level=all=v'
alias mpv-ytdl-reverse='mpv --ytdl-raw-options=playlist-reverse='
alias mutt-debug='mutt -d 2'
alias mv='mv -i'
alias npmg='npm -g'
alias od='od -Ax -tc'
alias odd='od -td1'
alias odo='od -to1'
alias odx='od -tx1'
alias patch0='patch -Np0'
alias patch1='patch -Np1'
alias ping-mtu='ping -M do -s 2000'
alias pulse-streams='pacmd list-sink-inputs'
alias qiv='qiv -uLtiGfl --vikeys'
alias rax='rax2'
alias rm='rm -I --one-file-system'
alias sd='systemctl'
alias sdu='systemctl --user'
alias se='sudoedit'
alias sed-all="sed -r -e 'H;1h;\$!d;x'"
alias ss='ss -napstu'
alias stat="stat -c '%A %a %h %U %G %s %y %N'"
alias sudo-off='sudo -K'
alias sudo-on='sudo -v'
alias tmux-killall='tmux-all kill-server'
alias top='top -d 1'
alias vgfull='valgrind --leak-check=full --show-reachable=yes'
alias w3m='w3m -v'
alias weechat-plain='weechat -d $(mktemp -d)'
alias wi='curl -sSLf http://wttr.in/ | head -n-3'
alias ytdl-playlist="youtube-dl --yes-playlist -o '~/download/_youtube-dl/%(playlist)s/[%(playlist_index)s] %(title)s'"
alias ytdl-stdout='youtube-dl -o -'

a() {
    local d=${1:-5m} ts; ts=$(command date -R)
    printf '%s ... alarm after %s\n' "$ts" "$d"
    sleep "${1:-5m}"
    echo 'Beep...'
    notify -u critical 'Beep...' "Time's up!"
    mpv --load-scripts=no --keep-open=no --loop-playlist=no --loop-file=5 \
        /usr/share/sounds/freedesktop/stereo/alarm-clock-elapsed.oga
}

anplay() {
    local outfile
    outfile=$(find ~/tmp -name 'asciinema-*' -print0 | sort -z | tail -zn1)
    [ -e "$outfile" ] && asciinema play "$outfile"
}

anrec() {
    local ts outfile
    ts=$(date +%F.%s)
    outfile="$HOME/tmp/asciinema-$ts.json"
    asciinema rec -w 2 "$outfile"
}

base() { export BASEDIR=${1:-$PWD}; }
unbase() { unset BASEDIR; }

cd() {
    if [ $# -eq 0 ]; then
        command cd "${BASEDIR:-${SHOME:-$HOME}}"
    else
        command cd "$@"
    fi
}

ctlseqs() {
    set -- ~/src/xterm-*
    [ -d "$1" ] && squashlns <"$1/ctlseqs.txt" | $PAGER
}

date() {
    [ $# -eq 0 ] && set -- -R
    command date "$@"
}

fn() {
    if [ $# -eq 0 ]; then
        declare -f | $PAGER
    else
        declare -f -- "$1" || return 1
        case $SHNAME in bash)
            (shopt -s extdebug; printf '# '; declare -F -- "$1")
        esac
    fi
}

h() {
    fc -- "${1:-1}" -1
}

i() {
    if [ $# -eq 0 ]; then
        shi
        return
    fi
    case $SHNAME in
        bash) type -a -- "$1" ;;
        zsh) type -af -- "$1" ;;
        *) type "$1" ;;
    esac
}

ifs() {
    printf '%s' "$IFS" | command od -An -ta -tx1
}

ifs0() {
    IFS=' 	''
'
}

lsmod() {
    pgx command lsmod "$@"
}

lsof_pid() {
    lsof -p "${1:-$$}"
}
alias lsof-pid='lsof_pid'

on() {
    [ $# -eq 0 ] && return 2
    local p; p=$(command -v "$1") || return 1
    shift
    eval "${*:-ls -la}" "$p"
}

optset() {
    if [ $# -eq 0 ]; then
        set +o
    else
        [ ${#1} -gt 1 ] && return 2
        case $- in *$1*) ;; *) return 1 ;; esac
    fi
}

pstree() {
    pgx command pstree -ahglnpsSuU "$@"
}

reexec() {
    local cmdline; cmdline=$(cmdline)
    eval exec "$cmdline"
}

reload() {
    . /etc/profile
    . ~/.profile
    case $SHNAME in
        bash) . ~/.bashrc ;;
        zsh) . ~/.zshrc ;;
        *) . ~/.shrc ;;
    esac
}

shi() {
    cmdline
    printf '$$: %s\n' "$$"
    printf 'PPID: %s\n' "$PPID"
    case $SHNAME in
        bash)
            printf 'PID: %s\n' "$BASHPID"
            printf 'SHLVL: %s\n' "$SHLVL"
            printf 'SUBSHELL: %s\n' "$BASH_SUBSHELL"
            printf 'VERSION: %s\n' "$BASH_VERSION"
            ;;
        zsh)
            # shellcheck disable=SC2154
            printf 'PID: %s\n' "${sysparams[pid]}"
            printf 'SHLVL: %s\n' "$SHLVL"
            printf 'SUBSHELL: %s\n' "$ZSH_SUBSHELL"
            printf 'VERSION: %s\n' "$ZSH_VERSION"
            ;;
    esac
}

source_opt() {
    if [ -e "$1" ]; then
        . -- "$1"
    fi
}

systemd_dot() {
    systemd-analyze dot "$@" | dot -Tsvg | stdiner -bt b
}
alias systemd-dot='systemd_dot'

tree() {
    set -- --dirsfirst -a -I '.git|.svn' --noreport -x "$@"
    if [ -t 1 ]; then
        pgx command tree -C "$@"
    else
        command tree "$@"
    fi
}

tsrec() {
    local ts outfile
    ts=$(date +%F.%s)
    outfile="$HOME/tmp/typescript-$ts"
    script -- "$outfile"
}

v() {
    if [ $# -eq 0 ]; then
        declare -p | $PAGER
    else
        declare -p -- "$1"
    fi
}

xkbkeymap() {
    pgx xkbcomp -a "$DISPLAY" -
}

xrandr() {
    if [ $# -eq 0 ] && [ -t 1 ]; then
        pgx command xrandr --verbose
    else
        command xrandr "$@"
    fi
}

xserver_log() {
    local dispno; dispno=${1:-$(xserverq dispno)}
    [ "$dispno" ] || return 1
    $PAGER ~/.local/share/xorg/Xorg."$dispno".log
}
alias xserver-log='xserver_log'

xsession_out() {
    local dispno; dispno=${1:-$(xserverq dispno)}
    [ "$dispno" ] || return 1
    $PAGER ~/.local/share/xorg/xsession."$dispno".out
}
alias xsession-out='xsession_out'

# ----------------------------------------

case $SHNAME in bash | zsh | *ksh) ;; *) return ;; esac

# virtualenvwrapper
if [ -e /usr/bin/virtualenvwrapper.sh ]; then
    . /usr/bin/virtualenvwrapper_lazy.sh

    PYTHON2=$(command -v python2)
    PYTHON3=$(command -v python3)
    export PYTHON2 PYTHON3
    [ "$PYTHON2" ] && mkvirtualenv2() { mkvirtualenv -p "$PYTHON2" "$@"; }
    [ "$PYTHON3" ] && mkvirtualenv3() { mkvirtualenv -p "$PYTHON3" "$@"; }
    alias wo='workon'

    mkvirtualenv_pyenv() {
        [ $# -eq 0 ] && return 2
        local ver=$1
        shift
        mkvirtualenv -p "$PYENV_ROOT/versions/$ver/bin/python" "$@"
    }
    alias mkvirtualenv-pyenv='mkvirtualenv_pyenv'
fi
