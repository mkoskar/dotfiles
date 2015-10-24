# ~/.profile
# - executed by the command interpreter for login shells
# - not read by bash(1), if ~/.bash_profile or ~/.bash_login exists
# :Compatibility: POSIX

[ "$SHRC_DEBUG" ] && echo '~/.profile' >&2

umask 022

export HOSTNAME=$(hostname)
export LANG='en_US.UTF-8'
export LC_COLLATE='C'
export LD_LIBRARY_PATH="$HOME/opt/lib"
export SHELL=${SHELL:-/bin/sh}

[ "$TERM" ] || TERM=dumb
export TERM

export TMPDIR="/tmp/$USER"
[ -d "$TMPDIR" ] || mkdir -p -m 700 "$TMPDIR"

eval "$(TERM=ansi dircolors)"

export JDK_HOME='/usr/lib/jvm/default'
export IDEA_JDK='/usr/lib/jvm/java-7-jdk'
export JAVA_HOME=$JDK_HOME
export _JAVA_AWT_WM_NONREPARENTING=1

export ASPROOT="$HOME/.cache/asp"
export BROWSER='b'
export EDITOR='e'
export GRADLE_HOME='/usr/share/java/gradle'
export GREP_COLORS='sl=:cx=:mt=41:fn=36:ln=33:bn=33:se=:ne'
export LESS='-MRS#3ci'
export LESSHISTFILE='-'
export LESSOPEN='| highlight --quiet -O xterm256 -s bluegreen %s'
export MANPAGER='manpg'
export ORACLE_HOME='/opt/instantclient'
export PACKER_CACHE_DIR="$HOME/.cache/packer"
export PAGER='pg'
export PARINIT='T4 w79 prbgqR B=.,?_A_a Q=_s>|'
export PYENV_ROOT="$HOME/opt/pyenv"
export SSH_AUTH_SOCK="$HOME/.ssh/S.ssh-agent"
export STDERRED_ESC_CODE=$(tput setaf 3)
export VDPAU_DRIVER='va_gl'
export XAUTHORITY="$HOME/.Xauthority"

[ -e /usr/lib/libstderred.so ] && export LD_PRELOAD='/usr/lib/libstderred.so'

[ "$_PATH" ] || export _PATH="$PATH"
PATH="$_PATH"
PATH="$HOME/.npm-global/bin:$PATH"
PATH="$HOME/.local/bin:$PATH"
PATH="$PYENV_ROOT/bin:$PATH"
PATH="$HOME/opt/bin:$PATH"
PATH="$HOME/projects/pub/dockerfiles/bin:$PATH"
PATH="$HOME/bin:$PATH"
export PATH
