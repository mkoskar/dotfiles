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

shmode() {
    case $SHNAME in
        bash) shopt -q -o posix && echo sh || echo bash ;;
        zsh) emulate ;;
        *ksh) echo ksh ;;
        *) echo sh ;;
    esac
}
SHMODE=$(shmode)

set -o noclobber
set -o notify
if [ "$SHMODE" = bash ]; then
    shopt -s expand_aliases
    unset BASH_ENV
fi
case $SHMODE in bash | zsh | ksh) set -o pipefail ;; esac

[ "$HOSTNAME" ] || HOSTNAME=${HOST:-$(uname -n)}
[ "$OSID" = termux ] && HOSTNAME=${TERMUX_HOST:-$HOSTNAME}

has() { command -v -- "$1"; } >/dev/null


# ansible
# ----------------------------------------

alias ansible-config-changed='ansible-config dump --only-changed'
alias ansible-config-help='ansible-config list'
alias ansible-facts='ansible -m setup'
alias ansible-hostvars='ansible -m debug -a var=hostvars'
alias ansible-inventory-graph='ansible-inventory --graph'
alias ansible-modules='ansible-doc -l'
alias ansible-playbook-tasks='ansible-playbook --list-tasks'


# curl
# ----------------------------------------

alias curl='\curl --proto -all,https --proto-default https -SLJR'

alias curl-as='curl -A "$UAGENT"'
alias curl-head='\curl -I'
alias curl-mkoskar='curl --netrc-file ~/.secrets/mkoskar.netrc'
alias curl-proxy-mkoskar='curl -K ~/.secrets/mkoskar-proxy.curlrc'
alias curl-socks='curl --preproxy socks5h://localhost:1080'
alias curl-time='curl -w "%{stderr}%{time_namelookup} namelookup\n%{time_connect} connect\n%{time_appconnect} appconnect\n%{time_pretransfer} pretransfer\n%{time_starttransfer} starttransfer\n%{time_posttransfer} posttransfer\n%{time_redirect} redirect\n%{time_total} total\n"'
alias curl-tor='curl --preproxy socks5h://localhost:9050'
alias curl-trace='curl --trace-ascii - --trace-time'


# docker / podman
# ----------------------------------------

alias dk=docker
alias dkc='docker container'
alias dke='docker exec -i -t'
alias dki='docker image'
alias dkr='docker run -P -i -t'

alias pd=podman
alias pdc='podman container'
alias pde='podman exec -i -t'
alias pdi='podman image'
alias pdr='podman run -P -i -t'

alias dk-prune='docker system prune'
alias pd-prune='podman system prune'

dk-stop() {
    docker container ls -aq | command xargs -rx docker stop
}

alias pd-stop='podman stop -a'


# dotfiles
# ----------------------------------------

alias .f=dotfiles

dotfiles() {
    [ $# -gt 0 ] || set -- status -s
    command git --git-dir ~/.dotfiles/.git --work-tree ~/ "$@"
}

dotfiles-init() {
    git clone -n https://github.com/mkoskar/dotfiles.git ~/.dotfiles
    dotfiles set-email
    dotfiles reset .
    dotfiles checkout-index -a
}


# find
# ----------------------------------------

alias f="find -regextype posix-extended -mindepth 1 ! \\( -type d -regex '.*/\\.(bzr|git|hg|svn)' -prune \\)"
alias fa='find -mindepth 1'
alias find-broken-links='find -xtype l'
alias find-stale-by-24h='find -mtime +1'
alias find-stale-by-day='find -daystart -mtime +1'


# gpg
# ----------------------------------------

alias gpg-connect-dirmngr='gpg-connect-agent --dirmngr'
alias gpg-debug='gpg -vv --debug-level=guru'
alias gpg-sandbox='gpg --homedir ~/.gnupg/sandbox'
alias gpg0='gpg --no-options'

alias gpg-agent-kill='gpg-connect-agent killagent /bye'
alias gpg-agent-pid="gpg-connect-agent 'getinfo pid' /bye"
alias gpg-agent-reload='gpg-connect-agent reloadagent /bye'
alias gpg-agent-sessionenv="gpg-connect-agent 'getinfo std_session_env' /bye"
alias gpg-agent-socket="gpg-connect-agent 'getinfo socket_name' /bye"
alias gpg-agent-updatetty='gpg-connect-agent updatestartuptty /bye'

alias gpg-dirmngr-kill='gpg-connect-dirmngr killdirmngr /bye'
alias gpg-dirmngr-reload='gpg-connect-dirmngr reloaddirmngr /bye'


# grep
# ----------------------------------------

alias grep='LC_ALL=C \grep --color=auto'

alias g='grep --color=always'
alias gi='g -i'
alias gr='g -rnH --exclude-dir=.git'
alias grepcat='grep --exclude-dir=\* .'
alias gri='gr -i'

alias ag='\ag --smart-case --noheading --nobreak --color-path=36 --color-line-number=33 --color-match=41'
alias rg='\rg --sort path -n --smart-case --no-heading --colors path:fg:6 --colors line:fg:3 --colors match:none --colors match:bg:1'


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

alias ls='\ls -F --group-directories-first --color=auto'

alias l='ls -1'
alias ll='ls -lh'

alias la='ll -A'
alias laa='ll -a'
alias lr='ll -R'
alias lt='ll -tr'
alias lta='lt -u'
alias ltc='lt -c'
alias lu='ll -Sr'
alias lx='ll -XB'


# man
# ----------------------------------------

alias manless='MANPAGER=less\ -s man'

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
alias paccheck-all='paccheck-modified --depends --opt-depends --file-properties --db-files'
alias paccheck-modified='paccheck --quiet --files --md5sum --sha256sum --require-mtree --backup --noextract --noupgrade'
alias pacdiff='\pacdiff -s -3'
alias pacman-log='pg /var/log/pacman.log'
alias pacq='pacman -Q'
alias pactree='\pactree --color'

_paclog_recent() {
    paclog --action=all | paclog --after="$(date -I -d -7days)" | $PAGER
}
alias paclog-recent=_paclog_recent

pacd() {
    [ $# -eq 0 ] && return 2
    expac -l \\n %D "$@"
}

pacdo() {
    [ $# -eq 0 ] && return 2
    expac -l \\n %o "$@"
}

paci() {
    [ $# -eq 0 ] && return 2
    pacman -Qii -- "$@" || pacman -Sii -- "$@" || aur search -i -- "$@"
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
    cmd -V -- "$@" | command xargs -rx -d \\n pacman -Qo
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
    sudo pacman -Rcs "$@"
}

pacs() {
    [ $# -eq 0 ] && return 2
    if [ -t 1 ]; then
        pacsearch "$1"
    else
        pacsearch --nocolor "$1"
    fi
}

pacss() {
    [ $# -eq 0 ] && return 2
    pacs "$1"
    aur search -- "$1"
}

pacw() {
    [ $# -eq 0 ] && return 2
    expac -l \\n %N "$@"
}


# python
# ----------------------------------------

alias py=python
alias ipy=ipython
alias venv='python -m venv'


# vim/nvim
# ----------------------------------------

alias vim-batch='vim -NXn -i NONE -u NONE -V1 -es'
alias nvim-batch='nvim -NXn -i NONE -u NONE -V1 -es'


# youtube-dl
# ----------------------------------------

#alias ytdl=youtube-dl
#alias ytdl-audio-playlist="ytdl-audio --yes-playlist -o '%(uploader)s/%(playlist)s - %(release_date>%Y)s/[%(playlist_index)s] %(title)s.%(ext)s'"
alias ytdl-audio-playlist="ytdl-audio --yes-playlist -o '%(playlist_uploader)s/%(playlist)s/[%(playlist_index)s] %(title)s.%(ext)s'"
alias ytdl-audio='ytdl -f bestaudio/best -x'
alias ytdl-best='ytdl -f bestvideo+bestaudio/best'
alias ytdl-formats='ytdl -F'
alias ytdl-json='ytdl -J'
alias ytdl-playlist="ytdl --yes-playlist -o '%(playlist_uploader)s/%(playlist)s/[%(playlist_index)s] %(title)s.%(ext)s'"
alias ytdl-stdout='ytdl -o -'
alias ytdl=yt-dlp


# Other
# ----------------------------------------

alias ..='cd ..'
alias ...='cd ../..'
alias ....='cd ../../..'

alias acpi='\acpi -V'
alias alarm-bread='alarm every 1h bread fold'
alias alarm-hourly='alarm at \*:0'
alias an=asciinema
alias aria-noalloc='aria2c -c -m 0 --file-allocation=none'
alias aria='aria2c -c -m 0'
alias aunpack='\aunpack -q'
alias avahi-browse='\avahi-browse -avtr'
alias c=calc
alias cal='\cal -3mw'
alias callgrind='valgrind --tool=callgrind'
alias caps='filecap -d'
alias clipi='clip -i'
alias cp='\cp -ai --reflink=auto'
alias cpio-copy='cpio -pdmv'
alias date-serial='date +%Y%m%d01'
alias date0='date -R'
alias dated='date -Id'
alias datee='date +%s'
alias datei='date -Is'
alias dateii='date +"Y%G Q%q W%V D%j"'
alias dconfa='dconf dump /'
alias dd='\dd bs=4M conv=fsync oflag=direct status=progress'
alias delv-nocheck='delv +cd'
alias delv-trace='delv +cd +tcp +mt +rt +vt'
alias delv='\delv +multiline +nocrypto'
alias df='\df -h -x devtmpfs -x tmpfs'
alias dig-nocheck='dig +cd'
alias dig-nssearch='dig +cd +tcp +nssearch'
alias dig-server-version='digs -c chaos -q version.bind -t txt'
alias dig-trace='dig +cd +tcp +trace'
alias dig-verbose='dig +qr +showbadcookie'
alias dig0='dig -r'
alias digs='dig +short'
alias dirs='\dirs -v'
alias dmesg='\dmesg -HTx'
alias dpms-off='xset dpms force off'
alias drill-nocheck='drill -o CD'
alias drill-trace='drill -o CD -tT'
alias dstat='\dstat -tlycgm --vm -rdn'
alias du='\du -hx'
alias dua='du --apparent-size -hx'
alias e0='e -Xn -i NONE -u NONE'
alias ebatch='e0 -V1 -es'
alias ed='\ed -v -p :'
alias fc-debug='FC_DEBUG=8191'
alias fc-recache='fc-cache -rv'
alias feh='\feh -F'
alias fortune='\fortune -c'
alias free='\free -h'
alias fuser='\fuser -v'
alias fzy='\fzy -l $LINES'
alias getfattr='\getfattr --absolute-names -d -m -'
alias glxgears-novsync='vblank_mode=0 glxgears'
alias gsettingsa='gsettings list-recursively'
alias gtk-debug='GTK_DEBUG=interactive'
alias host='\host -Tv'
alias info='\info --vi-keys'
alias infocmp0='infocmp -A "$SYSPREFIX"/share/terminfo'
alias infocmp='\infocmp -1a'
alias ip='\ip -h -p -c=auto'
alias journal-notice='journal -p 5'
alias journal='journalctl -o short-precise -b'
alias last='\last -x'
alias ld-debug='LD_DEBUG=all'
alias less-pager='PAGER=LESS'
alias llib='tree ~/.local/lib'
alias lsblk='\lsblk -o NAME,KNAME,MAJ:MIN,ROTA,RM,RO,TYPE,SIZE,FSTYPE,MOUNTPOINT,MODEL'
alias lsdiff='\lsdiff -s'
alias lsof-net-pid='lsof-net -a -p'
alias lsof-net='lsof -i'
alias lsof-pid='lsof -p'
alias lsof='\lsof -nP +c 0'
alias make-debug-all='make -d'
alias make-debug='make --debug=v,j,m,p,w'
alias make-force='make -B'
alias make-print='make -p -n'
alias make='\make -kwO --trace --warn-undefined-variables'
alias makepkg-build='makepkg -srf'
alias makepkg-rebuild='makepkg -Ccsrf'
alias me='lslogins "$USER"'
alias mnt='findmnt --real'
alias moon='curl -qf https://wttr.in/moon\?F'
alias mount-loop='mount -o loop'
alias mpv-debug='mpv --terminal=yes --msg-level=all=debug'
alias mpv-verbose='mpv --terminal=yes --msg-level=all=v'
alias mpv-ytdl-best='mpv --ytdl-format=bestvideo+bestaudio/best'
alias mpv-ytdl-reverse='mpv --ytdl-raw-options=playlist-reverse='
alias mpv0='mpv --no-config'
alias mv='\mv -i'
alias namei='\namei -l'
alias ncdu='\ncdu -x'
alias neomutt-debug='neomutt -d 5 -l ~/tmp/neomutt.log'
alias newsboat='\newsboat -q'
alias nmap-all='nmap -p 1-65535'
alias nopager='PAGER='
alias npmg='npm -g'
alias nslookup='\nslookup -vc -fail'
alias od='\od -A x -t c'
alias odd='od -t d1'
alias odo='od -t o1'
alias odx='od -t x1'
alias parallel-xargs='parallel -X -j1'
alias parallel='\parallel -rk -vv'
alias parallelq='parallel -q'
alias patch0='patch -N -p 0'
alias patch1='patch -N -p 1'
alias pax-copy='pax -rwv'
alias pdf-decrypt='qpdf --decrypt --remove-restrictions --password-file=-'
alias ping-mtu='ping -M do -s 2000'
alias pr='\pr -T -W "$COLUMNS"'
alias psmem='ps --format pid,%mem,pss:7,rss:7,sz:10,vsz:10,cmd --sort=-pss'
alias pwgen='\pwgen -cns'
alias qrdecode=zbarimg
alias qrencode-utf8='qrencode -t UTF8'
alias rax=rax2
alias reflector='\reflector -c cz,sk -p https -l 5 --sort rate'
alias rm='\rm -I --one-file-system'
alias rsync-ssl='rsync --rsh=rsync-rsh-openssl'
alias rsync-stdin='rsync --files-from=-'
alias rsync='\rsync -aHAXS -P -iv'
alias sarrec='sar -D -o ~/tmp'
alias sarsvg='sadf -g -O showinfo,showtoc ~/tmp --'
alias scp='\scp -rp'
alias sd-sysusers='systemd-sysusers --cat-config'
alias sd-tmpfiles='systemd-tmpfiles --cat-config'
alias sd=systemctl
alias sdu='systemctl --user'
alias sed-all="sed -e 'H;1h;\$!d;x'"
alias sed='\sed -E'
alias setxkbmap='\setxkbmap -v 10'
alias shred='\shred -v --random-source=/dev/urandom -n 1 -z'
alias smbclient='\smbclient --configfile=/dev/null'
alias socat='\socat -dd'
alias socati='socat readline,history=/dev/null'
alias speaker-test='\speaker-test -t wav -c 2'
alias srunX='srun -sX'
alias srunx='srun -sx'
alias ss='\ss -naptu'
alias ssh-cancel='ssh -O cancel'
alias ssh-check='ssh -O check'
alias ssh-exit='ssh -O exit'
alias ssh-forward='ssh -O forward'
alias ssh-socks='ssh -D 1080 -N'
alias ssh0='ssh -S none'
alias sshcat-cert='ssh-keygen -L -f'
alias sshcat-key='ssh-keygen -l -f'
alias stat="stat -c '%A %a %h %U %G %s %y %N'"
alias sys='systool -av'
alias systemd-debug='SYSTEMD_LOG_LEVEL=7 SYSTEMD_LOG_COLOR=1 SYSTEMD_LOG_TIME=1 SYSTEMD_LOG_LOCATION=1'
alias tcpdump='\tcpdump -ne -vvv'
alias tmux-killall='tmux-all kill-server'
alias tmux0='tmux -f /dev/null'
alias top='\top -d 1'
alias topdf='lowriter --convert-to pdf'
alias tp=tmux-pipe
alias tree='\tree -aF -I .git --dirsfirst --noreport'
alias treei='tree -pugshDif'
alias tshark-prefs='\tshark -G currentprefs'
alias tshark-protocols='\tshark -G protocols'
alias tshark='\tshark -nV'
alias vgfull='valgrind --leak-check=full --show-reachable=yes'
alias vpsfreectl-baobab='vpsfreectl vps remote_console 15499'
alias vpsfreectl-token='vpsfreectl --auth token --save --token-interval $((24*60*60)) user current'
alias w3m='\w3m -v'
alias watch='\watch -n 1'
alias weechat-tmux='tmux -L weechat attach'
alias weechat0='weechat -t'
alias weechat='\weechat -a'
alias whois='\whois --verbose --no-recursion -H'
alias wi='curl -qf https://wttr.in/\?Fqn'
alias xargs-lines='xargs -d \\n'
alias xargs-perline='xargs-lines -L 1'
alias xargs='\xargs -rx'
alias xev-keyboard='xev -event keyboard'
alias xhost-root='xhost +si:localuser:root'
alias xinput-test='xinput test-xi2 --root'
alias xwininfo='\xwininfo -all'
alias zip='\zip -r'
alias zsh0='zsh -df'

# Add/Remove the user from supplementary group(s)
alias usermod-addgroups='usermod -a -G'
alias usermod-rmgroups='usermod -r -G'
# addgroup/delgroup (busybox)
# gpasswd -a/-d

# Workaround for broken `udevadm` completion
alias udevadm-info-dev='udevadm info --name'
alias udevadm-info-sys='udevadm info --path'

alias _gdbus-session='gdbus introspect -r --session -o /'
alias _gdbus-system='gdbus introspect -r --system -o /'
alias gdbus-bluez='_gdbus-system -d org.bluez'
alias gdbus-login='_gdbus-system -d org.freedesktop.login1'

_() {
    git status -s .
}
alias ,=_

__() {
    local cwd gitdir
    cwd=$PWD
    case $cwd in "$HOME" | "$HOME"/*) cwd=\~${cwd##"$HOME"} ;; esac
    printf '\n%s @ %s in %s\n\n' "$USER" "$HOSTNAME" "$cwd"
    gitdir=$(git rev-parse --git-dir 2>/dev/null) || return 0
    case $gitdir in "$HOME" | "$HOME"/*) gitdir=\~${gitdir##"$HOME"} ;; esac
    echo --------------------------------------------------
    printf '> %s\n\n' "$gitdir"
    git conf
    echo
    echo --------------------------------------------------
    echo '> git-stash:'
    echo
    git -P stash list
    echo
}
alias ,,=__

base() {
    [ "$PWD" != "$HOME" ] || return
    export BASEDIR=${1:-$PWD}
}
unbase() { unset BASEDIR; }

bridge() {
    if [ $# -eq 0 ]; then
        ip link show type bridge
        ip link show type bridge_slave
    else
        command bridge -p -c=auto "$@"
    fi
}

# shellcheck disable=SC2164
cd() {
    case $SHNAME in zsh) setopt local_options posix_builtins ;; esac
    [ $# -gt 0 ] || set -- "${BASEDIR:-${SHOME:-$HOME}}"
    command cd -- "$@"
}

date() {
    [ $# -gt 0 ] || set -- +'%F %T'
    command date "$@"
}

if has fc; then
    fc() {
        case $SHNAME in zsh) setopt local_options posix_builtins ;; esac
        [ $# -gt 0 ] || set -- -- 1 "${HISTCMD:--1}"
        command fc "$@"
    }
fi

git() {
    [ $# -gt 0 ] || set -- status -s
    command git "$@"
}

fn() {
    if [ $# -eq 0 ]; then
        typeset -f | $PAGER
    else
        typeset -f -- "$@" || return 1
        case $SHNAME in bash) (shopt -s extdebug; typeset -F -- "$@") ;; esac
    fi
}

h() {
    case $SHNAME in
        zsh) fc -lD -t '%F %T' "$@" ;;
        *)
            if has fc; then
                fc -l "$@"
            else
                history | tail -n 16
            fi
            ;;
    esac
}

hread() {
    case $SHNAME in
        bash) history -r ;;
        zsh) fc -R ;;
        *) return 2 ;;
    esac
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

lsmod() {
    pgx command lsmod "$@"
}

on() {
    [ $# -eq 0 ] && return 2
    local p; p=$(command -v -- "$1") || return 1
    [ "${p##/*}" ] && { i "$1"; return; }
    shift
    eval "${*:-la}" "$(shell-escape "$p")"
}

optset() {
    if [ $# -eq 0 ]; then
        set +o
    else
        [ ${#1} -gt 1 ] && return 2
        case $- in *$1*) ;; *) return 1 ;; esac
    fi
}

path() { printf '%s\n' "$PATH" | tr : \\n; }
syspath() { printf '%s\n' "$SYSPATH" | tr : \\n; }

pstree() {
    pgx command pstree -ahglnpsSuU "$@"
}

r() {
    [ $# -gt 0 ] || set -- -1
    case $SHNAME in
        zsh) fc -L -e - "$@" ;;
        *) fc -e - "$@" ;;
    esac
}

reexec() {
    eval exec "$(cmdline)"
}

# shellcheck disable=SC1090,SC1091
reload() {
    . "$SYSETC"/profile
    . ~/.profile
    case $(shmode) in
        bash) . ~/.bashrc ;;
        zsh) . ~/.zshrc ;;
        *) . ~/.shrc ;;
    esac
}

shi() {
    printf '%s(%s): ' "$SHNAME" "$(shmode)"
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
    [ ! -e "$1" ] || . "$1"
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
