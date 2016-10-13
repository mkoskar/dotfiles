# Executed by login shells.
# Not read by bash(1), if ~/.bash_profile or ~/.bash_login exists.
# :Compatibility: POSIX

[ "$SHRC_DEBUG" ] && echo '~/.profile' >&2

umask 022

require() {
    [ "$2" ] && return
    printf '%s is not set or null\n' "$1" >&2
    exit 1
}

require HOME "$HOME"
require USER "$USER"
require LOGNAME "$LOGNAME"
require UID "$UID"

unset -f require

export LANG='en_US.UTF-8'
export LC_COLLATE='C'
export SHELL=${SHELL:-/bin/bash}
export XDG_RUNTIME_DIR="/run/user/$UID"

export TMPDIR="/tmp/$USER"
[ -e "$TMPDIR" ] || mkdir -m 700 "$TMPDIR"
if [ "$(stat --format '%a' "$TMPDIR")" != '700' ]; then
    printf '%s has wrong access rights\n' "$TMPDIR" >&2
    exit 1
fi

[ "$TERM" ] || TERM=dumb
export TERM

eval "$(TERM=ansi dircolors -b)"

export BROWSER='b'
export DIFFPROG='difftool'
export EDITOR='e'
export MANPAGER='manpg'
export PAGER='pg'
export TERMINAL='term'

export JAVA_HOME='/usr/lib/jvm/default-runtime'
export JDK_HOME='/usr/lib/jvm/default'
export _JAVA_AWT_WM_NONREPARENTING=1
export _JAVA_OPTIONS='-Dawt.useSystemAAFontSettings=on -Dsun.java2d.xrender=true -Dswing.aatext=true'

export ASPROOT="$HOME/.cache/asp"
export CCACHE_PATH='/usr/bin'
export DBUS_SESSION_BUS_ADDRESS="unix:path=$XDG_RUNTIME_DIR/bus"
export FZF_DEFAULT_OPTS='--no-mouse --bind=alt-k:page-up,alt-j:page-down'
export FZF_TMUX=0
export GRADLE_HOME='/usr/share/java/gradle'
export GREP_COLORS='sl=:cx=:mt=41:fn=36:ln=33:bn=33:se=:ne'
export GTK_IM_MODULE='xim'
export LD_LIBRARY_PATH="$HOME/opt/lib"
export LESS='-MRS#3ci'
export LESSHISTFILE='-'
export LESSOPEN='|highlight --quiet -O xterm256 -s bluegreen %s'
export MAKEFLAGS='-j2'
export NO_AT_BRIDGE=1
export ORACLE_HOME='/opt/instantclient'
export PACKER_CACHE_DIR="$HOME/.cache/packer"
export PARINIT='T4 w78 prbgqR B=.,?_A_a Q=_s>|'
export PYENV_ROOT="$HOME/opt/pyenv"
export PYTHONSTARTUP="$HOME/.pythonstartup"
export QT_IM_MODULE='xim'
export QUOTING_STYLE='literal'
export RANGER_LOAD_DEFAULT_RC='FALSE'
export SSH_AUTH_SOCK="$XDG_RUNTIME_DIR/ssh-agent"
export SYSTEMD_LESS=$LESS
export TERMINFO_DIRS='/etc/terminfo:/usr/share/terminfo'
export VDPAU_DRIVER='va_gl'
export VIMBIN='nvim'
export XAUTHORITY="$HOME/.local/share/xorg/Xauthority"

[ "$_PATH" ] || export _PATH="$PATH"
PATH="$_PATH"
PATH="$HOME/.cargo/bin:$PATH"
PATH="$HOME/.gem/ruby/2.3.0/bin:$PATH"
PATH="$HOME/.npm-global/bin:$PATH"
PATH="$HOME/.local/bin:$PATH"
PATH="$PYENV_ROOT/bin:$PATH"
PATH="$HOME/opt/bin:$PATH"
PATH="$HOME/projects/pub/dockerfiles/bin:$PATH"
PATH="$HOME/bin:$PATH"
export PATH
