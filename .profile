# Executed by login shells.
# Not read by bash(1), if ~/.bash_profile or ~/.bash_login exists.

[ "$SHRC_DEBUG" ] && echo ~/.profile >&2

export LANG=en_US.UTF-8
export LC_COLLATE=C
export LC_MEASUREMENT=C
export LC_PAPER=C

umask 022

if [ ! "$USER" ]; then
    USER=$(id -un)
    export USER
fi
export LOGNAME=$USER

require() {
    [ "$2" ] && return
    printf '%s is not set or null\n' "$1" >&2
    case $- in *i*) ;; *) exit 1 ;; esac
    exec /bin/bash --noprofile --norc -l -i
}

require HOME "$HOME"
require USER "$USER"

if [ ! "$UID" ]; then
    UID=$(id -u)
    export UID
fi

if [ -e /etc/os-release ]; then
    OSID=$(awk -F = '/^ID=/ { print $2 }' </etc/os-release)
elif [ -e /usr/lib/os-release ]; then
    OSID=$(awk -F = '/^ID=/ { print $2 }' </usr/lib/os-release)
fi
OSID=${OSID:-linux}
case $HOME in */com.termux/*) OSID=termux ;; esac
export OSID

case $OSID in
    termux)
        export SYSPREFIX=$PREFIX
        export SYSETC=$PREFIX/etc
        export TMPDIR=$HOME/tmp
        ;;
    *)
        export SYSPREFIX=/usr
        export SYSETC=/etc
        export TMPDIR=/tmp/$USER
        ;;
esac

export SHELL=${SHELL:-$SYSPREFIX/bin/bash}

[ -e "$TMPDIR" ] || mkdir -m 700 "$TMPDIR"
if [ "$(stat -c %a "$TMPDIR")" != 700 ]; then
    printf '%s has wrong access rights\n' "$TMPDIR" >&2
fi

export XDG_RUNTIME_DIR=/run/user/$UID
[ -e /run/user ] || export XDG_RUNTIME_DIR=$TMPDIR/run

[ -e "$XDG_RUNTIME_DIR" ] || mkdir -m 700 "$XDG_RUNTIME_DIR"
if [ -e "$XDG_RUNTIME_DIR" ] &&
   [ "$(stat -c %a "$XDG_RUNTIME_DIR")" != 700 ]; then
    printf '%s has wrong access rights\n' "$XDG_RUNTIME_DIR" >&2
fi

XDG_CACHE_HOME=~/.cache
XDG_CONFIG_HOME=~/.config
XDG_DATA_HOME=~/.local/share

mkdir -p "$XDG_CACHE_HOME" "$XDG_CONFIG_HOME" "$XDG_DATA_HOME"

[ "$TERM" ] || TERM=dumb
export TERM

if hash dircolors 2>/dev/null; then
    eval "$(TERM=ansi dircolors -b)"
fi

export BROWSER=b
export DIFFPROG=difftool
export EDITOR=e
export MANPAGER=manpg
export PAGER=pg
export TERMINAL=term

[ "$SYSPATH" ] || export SYSPATH=$PATH

addpath() {
    for i in "$@"; do
        [ -d "$1" ] && PATH=$i${PATH:+:$PATH}
    done
    unset i
}

# shellcheck disable=SC2123
PATH=

addpath "$SYSPREFIX"/local/bin/busybox
addpath ~/bin/busybox

PATH=$SYSPATH:$PATH

addpath ~/bin/system
addpath ~/.gem/ruby/*
addpath ~/.cargo/bin
addpath ~/.luarocks/bin
addpath ~/.local/pipx/bin
addpath ~/.local/bin
addpath ~/opt/bin
addpath ~/projects/pub/dockerfiles/bin
addpath ~/projects/pub/pkgbuilds
addpath ~/projects/pub/tcolors/bin
addpath ~/bin

export PATH

# ----------------------------------------

export AWT_TOOLKIT=MToolkit
export JAVA_HOME=$SYSPREFIX/lib/jvm/default-runtime
export JDK_HOME=$SYSPREFIX/lib/jvm/default
export _JAVA_AWT_WM_NONREPARENTING=1
export _JAVA_OPTIONS='-Dawt.useSystemAAFontSettings=on -Dsun.java2d.opengl=true -Dsun.java2d.xrender=true -Dswing.aatext=true'

#export SSH_AUTH_SOCK=${SSH_AUTH_SOCK:-$XDG_RUNTIME_DIR/gnupg/S.gpg-agent.ssh}
#export SSH_AUTH_SOCK=${SSH_AUTH_SOCK:-$XDG_RUNTIME_DIR/keyring/ssh}
export SSH_AUTH_SOCK=${SSH_AUTH_SOCK:-$XDG_RUNTIME_DIR/ssh-agent}

#export LESSOPEN='|highlight --quiet -O xterm256 -s bluegreen %s'
export ANSIBLE_COLOR_VERBOSE=yellow
export ASPROOT=~/.asp
export AUR_MAINTAINER=mkoskar
export BZR_LOG=$XDG_DATA_HOME/bzr.log
export CCACHE_DIR=$XDG_CACHE_HOME/ccache
export CCACHE_PATH=$SYSPREFIX/bin
export DBUS_SESSION_BUS_ADDRESS=unix:path=$XDG_RUNTIME_DIR/bus
export ENV=~/.shrc
export GRADLE_HOME=$SYSPREFIX/share/java/gradle
export GREP_COLORS=sl=:cx=:mt=41:fn=36:ln=33:bn=33:se=:ne
export GTK2_RC_FILES=$XDG_CONFIG_HOME/gtk-2.0/gtkrc
export GTK_IM_MODULE=xim
export GTK_MODULES=canberra-gtk-module
export LD_LIBRARY_PATH=~/opt/lib
export LESS=-AqRS#3ciPs
export LESSHISTFILE=-
export MAILDIR=~/mail
export MANSECT=0:9:2:3:7:8:6:1:4:5
export MANWIDTH=80
export MPLAYER_HOME=$XDG_CONFIG_HOME/mplayer
export NO_AT_BRIDGE=1
export PARALLEL_SHELL=$SYSPREFIX/bin/bash
export PARINIT='T4 w78 prbgqR B=.,?_A_a Q=_s>|'
export PERL_RL=o=0
export PIPENV_COLORBLIND=1
export PIPENV_HIDE_EMOJIS=1
export PIPENV_NOSPIN=1
export PIPENV_SHELL_FANCY=1
export PIPENV_VENV_IN_PROJECT=1
export PIPX_BIN_DIR=~/.local/pipx/bin
export PYENV_ROOT=~/.pyenv
export PYTHONSTARTUP=~/.pythonrc
export QT_IM_MODULE=xim
export QT_QPA_PLATFORMTHEME=qt5ct
export QUOTING_STYLE=literal
export RANGER_LOAD_DEFAULT_RC=FALSE
export SAL_USE_VCLPLUGIN=gtk
export SYSTEMD_LESS=$LESS
export SYSTEMD_PAGERSECURE=0
export S_COLORS=auto
export S_TIME_DEF_TIME=UTC
export S_TIME_FORMAT=ISO
export TERMINFO_DIRS=$SYSETC/terminfo:$SYSPREFIX/share/terminfo
export TMUX_TMPDIR=$TMPDIR
export UAGENT='Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/66.0.3359.139 Safari/537.36'
export VDPAU_DRIVER=va_gl
export VIMINIT='source ~/.vim/vimrc'
export XMOBAR_DATA_DIR=$XDG_DATA_HOME/xmobar
export XMONAD_DATA_DIR=$XDG_DATA_HOME/xmonad

if hash fuzz 2>/dev/null; then
    eval "$(fuzz -)"
fi

if hash luarocks 2>/dev/null; then
    eval "$(luarocks path --no-bin)"
fi

if hash nvim 2>/dev/null; then
    export VIMBIN=nvim
fi

# ----------------------------------------

source_opt() {
    # shellcheck disable=SC1090
    [ ! -e "$1" ] || {
        [ "$SHRC_DEBUG" ] && printf '%s\n' "$1" >&2
        . "$1"
    }
}

HOSTNAME=$(uname -n)
case $OSID in termux)
    # shellcheck disable=SC2155
    export TERMUX_HOST=$(getprop net.hostname)
    HOSTNAME=${TERMUX_HOST:-$HOSTNAME}
    ;;
esac
export HOSTNAME

source_opt ~/.profile_"$OSID"
source_opt ~/.profile."$HOSTNAME"

# ----------------------------------------

case $- in *i*) ;; *) return ;; esac

if [ -e ~/.hushlogin ] && [ -e "$SYSETC"/motd ]; then
    if ! cmp -s "$SYSETC"/motd ~/.hushmotd; then
        tee ~/.hushmotd <"$SYSETC"/motd
        echo '((( MOTD shown only once, unless it is changed )))'
    fi
fi
