# Source this file to get common aliases and functions.

[ "$SHRC_DEBUG" ] && echo ~/bin/shx.sh >&2

if [ "$BASH_VERSION" ]; then
    SHNAME=bash
elif [ "$ZSH_VERSION" ]; then
    SHNAME=zsh
elif [ "$KSH_VERSION" ]; then
    case $KSH_VERSION in
        *LEGACY*) SHNAME=lksh ;;
        *MIRBSD*) SHNAME=mksh ;;
        *PD*) SHNAME=pdksh ;;
        *) SHNAME=ksh ;;
    esac
else
    read -r SHNAME </proc/$$/comm
fi

# shellcheck disable=SC2034,SC2209
case $SHNAME in
    bash) shopt -q -o posix && SHMODE=sh || SHMODE=bash ;;
    zsh) SHMODE=$(emulate) ;;
    *ksh) SHMODE=ksh ;;
    *) SHMODE=sh ;;
esac

# ----------------------------------------

set -o noclobber
case $SHNAME in bash)
    shopt -s expand_aliases
    unset BASH_ENV
    ;;
esac
case $SHNAME in bash | zsh | *ksh) set -o pipefail ;; esac

# shellcheck disable=SC2153
case $SHNAME in
    bash) ;;
    zsh) HOSTNAME=$HOST ;;
    *) HOSTNAME=$(uname -n) ;;
esac
case $OSID in termux) HOSTNAME=${TERMUX_HOST:-$HOSTNAME} ;; esac


# docker
# ----------------------------------------

alias dk=docker
alias dkb='docker build'
alias dkc='docker ps'
alias dkca='docker ps -a'
alias dkcl='docker ps -lq'
alias dke='docker exec -it'
alias dki='docker images'
alias dkia='docker images -a'
alias dkr='docker run -P'
alias dkrd='docker run -dP'
alias dkri='docker run -itP'

dkip() {
    local target; target=${1:-$(docker ps -lq)}
    [ "$target" ] || return 2
    docker inspect --format '{{ .NetworkSettings.IPAddress }}' "$target"
}

dkrm() {
    confirm 'Remove ALL containers (with volumes). Continue?' n || return 0
    docker ps -aq | xargs -rx docker rm -vf
}

dkstop() {
    docker ps -aq | xargs -rx docker stop
}


# grep
# ----------------------------------------

alias grep='LC_ALL=C grep --color=auto'
alias g='grep --color=always'
alias gi='g -i'

gr() {
    g -rnH --exclude-dir=.git "$@" | sort -i -t : -k 1,1
}
alias gri='gr -i'

alias ag='ag --smart-case --noheading --nobreak --color-path=36 --color-line-number=33 --color-match=41'
alias rg='rg --sort path -n --smart-case --no-heading --colors path:fg:6 --colors line:fg:3 --colors match:none --colors match:bg:1'


# java
# ----------------------------------------

alias gradle-dependencies='gradle -q dependencies'
alias gradle-tasks='gradle -q tasks --all'
alias groovy-grape-verbose='groovy -Dgroovy.grape.report.downloads=true'
alias java-info='java -XshowSettings:all -version'
alias mvn-dependency-tree='mvn dependency:tree'
alias mvn-effective-pom='mvn help:effective-pom'
alias mvn-effective-settings='mvn help:effective-settings'

_mvn_archetype_generate() {
    mvn archetype:generate -Dfilter="$1"
}
alias mvn-archetype-generate=_mvn_archetype_generate

_mvn_describe_plugin() {
    [ $# -eq 0 ] && return 2
    mvn help:describe -Dplugin="$1"
}
alias mvn-describe-plugin=_mvn_describe_plugin


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
alias man-posix='man -s 0p,9p,2p,3p,7p,8p,6p,1p,4p,5p'

_man_all() {
    pgx man -k . "$@"
}
alias man-all=_man_all

alias man-all-1p='man-all -s 1p'
alias man-all-3p='man-all -s 3p'
alias man-all-posix='man-all -s 0p,9p,2p,3p,7p,8p,6p,1p,4p,5p'


# pacman
# ----------------------------------------

alias pac=pacman
alias paccheck='paccheck --quiet --depends --opt-depends --files --file-properties --sha256sum --require-mtree --db-files --backup --noextract --noupgrade'
alias pacman-log='pg /var/log/pacman.log'
alias pactree='pactree --color'

_paclog_recent() {
    paclog --action=all | paclog --after="$(date -I -d -7days)"
}
alias paclog-recent=_paclog_recent

pacd() {
    [ $# -eq 0 ] && return 2
    expac -l \\n %D "$@"
}

paci() {
    [ $# -eq 0 ] && return 2
    pacman -Qii -- "$@" || pacman -Sii -- "$@" || auracle info -- "$@"
} 2>/dev/null

pacl() {
    [ $# -eq 0 ] && return 2
    pacman -Qql -- "$@" || pacman -Fql -- "$@"
} 2>/dev/null

paco() {
    [ $# -eq 0 ] && return 2
    pacman -Qo -- "$@"
}

pacoc() {
    [ $# -eq 0 ] && return 2
    pth -a "$@" | xargs -r -L 1 pacman -Qo
}

pacp() {
    if [ $# -eq 0 ]; then
        expac -l ' ' '%n %P'
    else
        expac -l \\n %P "$@"
    fi
}

pacrm() {
    [ $# -eq 0 ] && return 2
    confirm 'Careful! Continue?' n || return 0
    sudo pacman -Rcsn "$@"
}

pacs() {
    [ $# -eq 0 ] && return 2
    pacsearch "$1"
    auracle --color=always search -- "$1"
}

pacw() {
    [ $# -eq 0 ] && return 2
    expac -l \\n %N "$@"
}


# python
# ----------------------------------------

alias py=python
alias ipy=ipython


# Other
# ----------------------------------------

alias ..='cd ..'
alias ...='cd ../..'
alias ....='cd ../../..'

alias aa=auracle
alias acpi='acpi -V'
alias an=asciinema
alias aunpack='aunpack -q'
alias c=calc
alias cal='cal -3mw'
alias callgrind='valgrind --tool=callgrind'
alias cmd=command
alias cp='cp -ai --reflink=auto'
alias curl-as='curl -A "$UAGENT"'
alias date0='date -Ins'
alias dconfa='dconf dump /'
alias dd='dd status=progress'
alias delv-trace='delv +cd +mt +rt +vt -t any'
alias delv='delv +cd -t any'
alias df0='\df -h'
alias df='df -h -x tmpfs -x devtmpfs'
alias dig-nocheck='dig +cd'
alias dig-nssearch='dig +tcp +cd +nssearch'
alias dig-trace='dig +tcp +trace'
alias dirs='dirs -v'
alias dmesg='dmesg -HTx'
alias dpms-off='xset dpms force off'
alias drill-nocheck='drill -o CD'
alias drill-trace='drill -T'
alias dstat='dstat -cglmnpry --tcp'
alias du='du -hx'
alias ed='ed -p :'
alias feh='feh -F'
alias fortune='fortune -c'
alias free='free -h'
alias fzy='fzy -l $LINES'
alias gconfa='gconftool-2 -R /'
alias glxgears-novsync='vblank_mode=0 glxgears'
alias gpg-agent-reload='gpg-connect-agent reloadagent /bye'
alias gpg-agent-sessionenv="gpg-connect-agent 'getinfo std_session_env' /bye"
alias gpg-agent-updatetty='gpg-connect-agent updatestartuptty /bye'
alias gpg-sandbox='gpg --homedir ~/.gnupg/sandbox'
alias grepcat='grep --exclude-dir=\* .'
alias gsettingsa='gsettings list-recursively'
alias headcat='head -v -n -0'
alias host='host -a -T'
alias info='info --vi-keys'
alias infocmp0='infocmp -A /usr/share/terminfo'
alias infocmp='infocmp -1a'
alias journal='journalctl -o short-precise -b'
alias llib='tree ~/.local/lib'
alias lsblk='lsblk -o NAME,KNAME,MAJ:MIN,ROTA,RM,RO,TYPE,SIZE,FSTYPE,MOUNTPOINT,MODEL'
alias lsdiff='lsdiff -s'
alias ltime='date +%T'
alias makepkg-build='makepkg -srf'
alias makepkg-rebuild='makepkg -Ccsrf'
alias mnt='findmnt --real'
alias moon='curl -sSLf http://wttr.in/moon | head -n -4'
alias mount-loop='mount -o loop'
alias mpv-debug='mpv --terminal=yes --msg-level=all=debug'
alias mpv-verbose='mpv --terminal=yes --msg-level=all=v'
alias mpv-ytdl-reverse='mpv --ytdl-raw-options=playlist-reverse='
alias mpv='mpv --player-operation-mode=pseudo-gui'
alias mv='mv -i'
alias neomutt-debug='neomutt -d 5 -l ~/tmp/neomutt.log'
alias nmap-all='nmap -p 1-65535'
alias npmg='npm -g'
alias nslookup='nslookup -vc -fail -type=any'
alias od='od -A x -t c'
alias odd='od -t d1'
alias odo='od -t o1'
alias odx='od -t x1'
alias parallel='parallel -r'
alias patch0='patch -N -p 0'
alias patch1='patch -N -p 1'
alias ping-mtu='ping -M do -s 2000'
alias qiv='qiv -uLtiGfl --vikeys'
alias rax=rax2
alias reflector='reflector -p https -c sk -c cz --score 3 -f 3'
alias rm='rm -I --one-file-system'
alias rsync='rsync -aHAXS'
alias scp='scp -rp'
alias sd=systemctl
alias sdu='systemctl --user'
alias se=sudoedit
alias sed-all="sed -r -e 'H;1h;\$!d;x'"
alias socat='socat readline,history=/dev/null'
alias ss='ss -napstu'
alias ssh0='ssh -S none'
alias sslcon='openssl s_client -showcerts -connect'
alias stat="stat -c '%A %a %h %U %G %s %y %N'"
alias sudo-off='sudo -K'
alias sudo-on='sudo -v'
alias sys='systool -av'
alias tcpdump='tcpdump -tttne'
alias tmux-killall='tmux-all kill-server'
alias top='top -d 1'
alias topdf='lowriter --convert-to pdf'
alias udevadm-info-dev='udevadm info --name'
alias udevadm-info-sys='udevadm info --path'
alias vgfull='valgrind --leak-check=full --show-reachable=yes'
alias w3m='w3m -v'
alias watch='watch -n 1'
alias weechat-tmux='tmux -L weechat attach'
alias weechat='weechat -a'
alias whois='whois -H'
alias wi='curl -sSLf http://wttr.in/ | head -n -2'
alias xargs1='xargs -r -L 1'
alias xinput-test='xinput test-xi2 --root'
alias ytdl-audio-album="ytdl-audio --yes-playlist -o '%(playlist_uploader)s/%(playlist)s/[%(playlist_index)s] %(title)s.%(ext)s'"
alias ytdl-audio-all='ytdl-audio --yes-playlist'
alias ytdl-audio='youtube-dl -f bestaudio/best -x'
alias ytdl-formats='youtube-dl -F'
alias ytdl-json='youtube-dl -J'
alias ytdl-playlist="youtube-dl --yes-playlist -o '%(playlist_uploader)s/%(playlist)s/[%(playlist_index)s] %(title)s.%(ext)s'"
alias ytdl-stdout="youtube-dl -f 'best[height<=?1080]' -o -"

_() {
    git status -s
}
alias ,=_

__() {
    local cwd gitdir
    cwd=$PWD
    case $cwd in "$HOME" | "$HOME"/*) cwd=\~${cwd##$HOME} ;; esac
    printf '\n%s @ %s in %s\n\n' "$USER" "$HOSTNAME" "$cwd"
    # shellcheck disable=SC2154
    if [ "$__long_cmd" ]; then
        printf '$ %s\n%s (%d sec)\n\n' \
            "$__long_cmd" \
            "$(date -R -d @"$__long_cmd_start")" \
            "$__long_cmd_dur"
    fi
    gitdir=$(git rev-parse --git-dir 2>/dev/null) || return 0
    case $gitdir in "$HOME" | "$HOME"/*) gitdir=\~${gitdir##$HOME} ;; esac
    echo '--------------------------------------------------'
    printf '> %s\n\n' "$gitdir"
    git conf
    echo $'\n--------------------------------------------------'
    echo $'> git-stash:\n'
    git -P stash list
    echo
}
alias ,,=__

_lsof_pid() {
    lsof -p "${1:-$$}"
}
alias lsof-pid=_lsof_pid

_systemd_dot() {
    systemd-analyze dot "$@" | dot -T svg | stdiner -bt b
}
alias systemd-dot=_systemd_dot

_terminfo_src() {
    set -- ~/src/ncurses-*
    [ -d "$1" ] && pg "$1"/misc/terminfo.src
}
alias terminfo-src=_terminfo_src

_xserver_log() {
    local dispno; dispno=$(env ${1+DISPLAY=:"$1"} xserverq dispno) || return 1
    local logfile=~/.local/share/xorg/Xorg:"$dispno".log
    [ -e "$logfile" ] || return 1
    $PAGER "$logfile"
}
alias xserver-log=_xserver_log

_xserver_reset() {
    confirm 'Careful! Continue?' n || return 0
    local pid; pid=$(env ${1+DISPLAY=:"$1"} xserverq pid) || return 1
    kill -s HUP "$pid"
}
alias xserver-reset=_xserver_reset

_xserver_terminate() {
    confirm 'Careful! Continue?' n || return 0
    local pid; pid=$(env ${1+DISPLAY=:"$1"} xserverq pid) || return 1
    kill "$pid"
}
alias xserver-terminate=_xserver_terminate

_xsession_out() {
    local screen; screen=$(env ${1+DISPLAY=:"$1"} xserverq screen) || return 1
    local outfile=~/.local/share/xorg/xsession:"$screen".out
    [ -e "$outfile" ] || return 1
    $PAGER "$outfile"
}
alias xsession-out=_xsession_out

a() {
    local d=${1:-5m} ts; ts=$(command date -R)
    printf '%s ... alarm in %s\n' "$ts" "$d"
    sleep "${1:-5m}"
    echo Beep...
    notify -u critical "Beep... Time's up!"
    command mpv --load-scripts=no --loop-file=5 \
        /usr/share/sounds/freedesktop/stereo/alarm-clock-elapsed.oga
}

anplay() {
    local outfile
    outfile=$(find ~/tmp -name asciinema-\* -print0 | sort -z | tail -z -n 1)
    [ -e "$outfile" ] && asciinema play "$outfile"
}

anrec() {
    local ts outfile
    ts=$(date +%F.%s)
    outfile=~/tmp/asciinema-"$ts".json
    asciinema rec -w 2 "$outfile"
}

base() { export BASEDIR=${1:-$PWD}; }
unbase() { unset BASEDIR; }

cd() {
    if [ $# -eq 0 ]; then
        command cd -- "${BASEDIR:-${SHOME:-$HOME}}"
    else
        command cd -- "$@"
    fi
}

ctlseqs() {
    set -- ~/src/xterm-*
    [ -d "$1" ] && squashlns <"$1"/ctlseqs.txt | $PAGER
}

date() {
    [ $# -eq 0 ] && set -- -R
    command date "$@"
}

fn() {
    if [ $# -eq 0 ]; then
        typeset -f | $PAGER
    else
        typeset -f -- "$@" || return 1
        case $SHNAME in bash)
            (shopt -s extdebug; typeset -F -- "$@")
        esac
    fi
}

h() {
    fc -- "${1:-1}" -1
}

i() {
    [ $# -gt 0 ] || { shi; return; }
    case $SHNAME in
        bash) type -a -- "$@" ;;
        zsh) type -af -- "$@" ;;
        *) type "$@" ;;
    esac
}

ifs() {
    printf %s "$IFS" | command od -A n -t a -t x1
}

ifs0() {
    IFS=' 	''
'
}

# shellcheck disable=SC2120
lsmod() {
    pgx command lsmod "$@"
}

on() {
    [ $# -eq 0 ] && return 2
    local p; p=$(command -v -- "$1") || return 1
    [ -e "$p" ] || { i "$1"; return; }
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

path() {
    printf '%s\n' "$PATH" | tr : \\n
}

pstree() {
    pgx command pstree -ahglnpsSuU "$@"
}

reexec() {
    local cmdline; cmdline=$(cmdline)
    eval exec "$cmdline"
}

# shellcheck disable=SC1090,SC2153
reload() {
    case $OSID in
        termux) . "$SYSPREFIX"/etc/profile ;;
        *) . /etc/profile ;;
    esac
    . ~/.profile
    case $SHMODE in
        bash) . ~/.bashrc ;;
        zsh) . ~/.zshrc ;;
        *) . ~/.shrc ;;
    esac
}

shi() {
    printf '%s(%s): ' "$SHNAME" "$SHMODE"
    cmdline
    printf '$-: %s\n' "$-"
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
            printf 'VERSION: %s (%s)\n' "$ZSH_VERSION" "$ZSH_PATCHLEVEL"
            ;;
        mksh)
            printf 'PID: %s\n' "$BASHPID"
            printf 'SHLVL: %s\n' "$SHLVL"
            printf 'VERSION: %s\n' "$KSH_VERSION"
            ;;
        *ksh)
            printf 'SHLVL: %s\n' "$SHLVL"
            printf 'VERSION: %s\n' "$KSH_VERSION"
            ;;
    esac
}

source_opt() {
    # shellcheck disable=SC1090
    [ ! -e "$1" ] || . -- "$1"
}

tree() {
    set -- -ax -I .git --dirsfirst --noreport "$@"
    if [ -t 1 ]; then
        pgx command tree -C "$@"
    else
        command tree "$@"
    fi
}

tsrec() {
    local ts outfile
    ts=$(date +%F.%s)
    outfile=~/tmp/typescript-"$ts"
    script -- "$outfile"
}

v() {
    case $SHNAME in bash | zsh | *ksh)
        if [ $# -eq 0 ]; then
            typeset -p | $PAGER
        else
            typeset -p -- "$@"
        fi
        return
        ;;
    esac
    set | $PAGER
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
