# Executed by login shells.
# Not read by bash(1), if ~/.bash_profile or ~/.bash_login exists.
# :Compatibility: POSIX

[ "$SHRC_DEBUG" ] && echo \~/.profile >&2

umask 022

require() {
    [ "$2" ] && return
    printf '%s is not set or null\n' "$1" >&2
    case $- in *i*) ;; *) exit 1 ;; esac
    exec /bin/bash --noprofile --norc -l -i
}

require HOME "$HOME"
require USER "$USER"
require PATH "$PATH"
require LOGNAME "$LOGNAME"

unset -f require

if [ ! "$UID" ]; then
    UID=$(id -u)
    export UID
fi

export LANG=en_US.UTF-8
export LC_COLLATE=C
export LC_MEASUREMENT=C
export LC_PAPER=C

export SHELL=${SHELL:-/bin/bash}
export XDG_RUNTIME_DIR=/run/user/$UID

export TMPDIR=/tmp/$USER
[ -e "$TMPDIR" ] || mkdir -m 700 "$TMPDIR"
if [ "$(stat -c '%a' "$TMPDIR")" != 700 ]; then
    printf '%s has wrong access rights\n' "$TMPDIR" >&2
    exit 1
fi

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

export JAVA_HOME=/usr/lib/jvm/default-runtime
export JDK_HOME=/usr/lib/jvm/default
export _JAVA_AWT_WM_NONREPARENTING=1
export _JAVA_OPTIONS='-Dawt.useSystemAAFontSettings=on -Dsun.java2d.opengl=true -Dsun.java2d.xrender=true -Dswing.aatext=true'

export AUR_MAINTAINER=mkoskar
export CCACHE_PATH=/usr/bin
export DBUS_SESSION_BUS_ADDRESS=unix:path=$XDG_RUNTIME_DIR/bus
export ENV=~/.shrc
export GRADLE_HOME=/usr/share/java/gradle
export GREP_COLORS=sl=:cx=:mt=41:fn=36:ln=33:bn=33:se=:ne
export GTK_IM_MODULE=xim
export LD_LIBRARY_PATH=~/opt/lib
export LESS=-AqRS#3ciPs
export LESSHISTFILE=-
export LESSOPEN='|highlight --quiet -O xterm256 -s bluegreen %s'
export MAILDIR=~/mail
export MANSECT=0:9:2:3:7:8:6:1:4:5
export NNTPSERVER=snews://news.eternal-september.org
export NO_AT_BRIDGE=1
export ORACLE_HOME=/opt/instantclient
export PARALLEL_SHELL=/bin/bash
export PARINIT='T4 w78 prbgqR B=.,?_A_a Q=_s>|'
export PYENV_ROOT=~/opt/pyenv
export PYTHONSTARTUP=~/.pythonrc
export QT_IM_MODULE=xim
export QT_QPA_PLATFORMTHEME=gtk2
export QUOTING_STYLE=literal
export RANGER_LOAD_DEFAULT_RC=FALSE
export SAL_USE_VCLPLUGIN=gtk
export SSH_AUTH_SOCK=${SSH_AUTH_SOCK:-$XDG_RUNTIME_DIR/ssh-agent}
export SYSTEMD_LESS=$LESS
export TERMINFO_DIRS=/etc/terminfo:/usr/share/terminfo
export TMUX_TMPDIR=$TMPDIR
export UAGENT='Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/66.0.3359.139 Safari/537.36'
export VDPAU_DRIVER=va_gl

if hash fuxx 2>/dev/null; then
    eval "$(fuzz -)"
fi

if hash luarocks 2>/dev/null; then
    eval "$(luarocks path)"
fi

if hash nvim 2>/dev/null; then
    export VIMBIN=nvim
fi

XDG_CACHE_HOME=~/.cache
XDG_CONFIG_HOME=~/.config
XDG_DATA_HOME=~/.local/share

mkdir -p "$XDG_CACHE_HOME" "$XDG_CONFIG_HOME" "$XDG_DATA_HOME"

export ASPROOT=~/.asp
export BZR_LOG=$XDG_DATA_HOME/bzr.log
export CCACHE_DIR=$XDG_CACHE_HOME/ccache
export GTK2_RC_FILES=$XDG_CONFIG_HOME/gtk-2.0/gtkrc
export MPLAYER_HOME=$XDG_CONFIG_HOME/mplayer
export NOTMUCH_CONFIG=$XDG_CONFIG_HOME/notmuch
export XAUTHORITY=$XDG_DATA_HOME/xorg/Xauthority

[ "$_PATH" ] || export _PATH=$PATH
PATH=$_PATH

set -- ~/.gem/ruby/*
[ -d "$1" ] && PATH=$1/bin:$PATH

PATH=~/.cargo/bin:$PATH
PATH=~/.luarocks/bin:$PATH
PATH=$PYENV_ROOT/bin:$PATH

PATH=~/.local/bin:$PATH
PATH=~/opt/bin:$PATH
PATH=~/projects/pub/dockerfiles/bin:$PATH
PATH=~/projects/pub/pkgbuilds:$PATH
PATH=~/projects/pub/tcolors/bin:$PATH

PATH=~/bin:$PATH
export PATH
