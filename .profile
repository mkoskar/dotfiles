# Executed by login shells.
# Not read by bash(1), if ~/.bash_profile or ~/.bash_login exists.
# :Compatibility: POSIX

[ "$SHRC_DEBUG" ] && echo '~/.profile' >&2

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

export LANG='en_US.UTF-8'
export LC_COLLATE='C'
export LC_MEASUREMENT='C'
export LC_PAPER='C'

export SHELL=${SHELL:-/bin/bash}
export XDG_RUNTIME_DIR="/run/user/$UID"

export TMPDIR="/tmp/$USER"
[ -e "$TMPDIR" ] || mkdir -m 700 "$TMPDIR"
if [ "$(stat -c '%a' "$TMPDIR")" != '700' ]; then
    printf '%s has wrong access rights\n' "$TMPDIR" >&2
    exit 1
fi

[ "$TERM" ] || TERM=dumb
export TERM

if hash dircolors 2>/dev/null; then
    eval "$(TERM=ansi dircolors -b)"
fi

export BROWSER='b'
export DIFFPROG='difftool'
export EDITOR='e'
export MANPAGER='manpg'
export PAGER='pg'
export TERMINAL='term'

export JAVA_HOME='/usr/lib/jvm/default-runtime'
export JDK_HOME='/usr/lib/jvm/default'
export _JAVA_AWT_WM_NONREPARENTING=1
export _JAVA_OPTIONS='-Dawt.useSystemAAFontSettings=on -Dsun.java2d.opengl=true -Dsun.java2d.xrender=true -Dswing.aatext=true'

export AUR_MAINTAINER='mkoskar'
export CCACHE_PATH='/usr/bin'
export DBUS_SESSION_BUS_ADDRESS="unix:path=$XDG_RUNTIME_DIR/bus"
export ENV="$HOME/.shrc"
export FZF_DEFAULT_OPTS='--no-mouse --tabstop=4 --inline-info --color=hl:3,hl+:214,info:-1,prompt:10,pointer:10,marker:10 --bind=alt-j:half-page-down,alt-k:half-page-up,ctrl-b:beginning-of-line,alt-h:backward-char,alt-l:forward-char,alt-w:forward-word,ctrl-k:kill-line,alt-x:delete-char,ctrl-o:jump,ctrl-j:accept'
export FZF_TMUX=0
export GRADLE_HOME='/usr/share/java/gradle'
export GREP_COLORS='sl=:cx=:mt=41:fn=36:ln=33:bn=33:se=:ne'
export GTK_IM_MODULE='xim'
export LD_LIBRARY_PATH="$HOME/opt/lib"
export LESS='-MRS#3ci'
export LESSHISTFILE='-'
export LESSOPEN='|highlight --quiet -O xterm256 -s bluegreen %s'
export NNTPSERVER='snews://news.eternal-september.org'
export NO_AT_BRIDGE=1
export ORACLE_HOME='/opt/instantclient'
export PARALLEL_SHELL='/bin/bash'
export PARINIT='T4 w78 prbgqR B=.,?_A_a Q=_s>|'
export PYENV_ROOT="$HOME/opt/pyenv"
export PYTHONSTARTUP="$HOME/.pythonrc"
export QT_IM_MODULE='xim'
export QT_QPA_PLATFORMTHEME='gtk2'
export QUOTING_STYLE='literal'
export RANGER_LOAD_DEFAULT_RC='FALSE'
export SAL_USE_VCLPLUGIN='gtk'
export SSH_AUTH_SOCK=${SSH_AUTH_SOCK:-$XDG_RUNTIME_DIR/ssh-agent}
export SYSTEMD_LESS=$LESS
export TERMINFO_DIRS='/etc/terminfo:/usr/share/terminfo'
export TMUX_TMPDIR=$TMPDIR
export VDPAU_DRIVER='va_gl'

if hash nvim 2>/dev/null; then
    export VIMBIN='nvim'
fi

XDG_CACHE_HOME="$HOME/.cache"
XDG_CONFIG_HOME="$HOME/.config"
XDG_DATA_HOME="$HOME/.local/share"

mkdir -p "$XDG_CACHE_HOME" "$XDG_CONFIG_HOME" "$XDG_DATA_HOME"

export ASPROOT="$HOME/.asp"
export BZR_LOG="$XDG_DATA_HOME/bzr.log"
export CCACHE_DIR="$XDG_CACHE_HOME/ccache"
export GTK2_RC_FILES="$XDG_CONFIG_HOME/gtk-2.0/gtkrc"
export MPLAYER_HOME="$XDG_CONFIG_HOME/mplayer"
export XAUTHORITY="$XDG_DATA_HOME/xorg/Xauthority"

[ "$_PATH" ] || export _PATH=$PATH
PATH=$_PATH
PATH="$HOME/.cargo/bin:$PATH"
PATH="$HOME/.gem/ruby/2.3.0/bin:$PATH"
PATH="$HOME/.local/bin:$PATH"
PATH="$PYENV_ROOT/bin:$PATH"
PATH="$HOME/opt/bin:$PATH"
PATH="$HOME/projects/pub/dockerfiles/bin:$PATH"
PATH="$HOME/projects/pub/pkgbuilds:$PATH"
PATH="$HOME/projects/pub/tcolors/bin:$PATH"
PATH="$HOME/bin:$PATH"
export PATH
