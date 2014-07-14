# ~/.profile
# - executed by the command interpreter for login shells
# - not read by bash(1), if ~/.bash_profile or ~/.bash_login exists

umask 022

export PATH="$HOME/.local/bin:$PATH"
export PATH="$HOME/opt/bin:$PATH"
export PATH="$HOME/projects/pub/tcolors/bin:$PATH"
export PATH="$HOME/bin:$PATH"

export LANG='en_US.UTF-8'
export LC_COLLATE='C'
export LD_LIBRARY_PATH="$HOME/opt/lib"

export BROWSER='firefox'
export DBUS_SESSION_BUS_ADDRESS="unix:path=$XDG_RUNTIME_DIR/bus"
export EDITOR='e'
export LESS='-MRS#3ci'
export LESSHISTFILE='-'
export LESSOPEN='| highlight --quiet -O xterm256 -s bluegreen %s'
export MAILCHECK=0
export MANPAGER='manpg'
export ORACLE_HOME='/opt/instantclient'
export PACKER_CACHE_DIR="$HOME/.packer_cache"
export PAGER='pg'
export VDPAU_DRIVER='va_gl'
export XAUTHORITY="$HOME/.Xauthority"

eval "$(TERM=ansi dircolors)"

export JDK_HOME="$JAVA_HOME"
_JAVA_OPTIONS='-Dawt.useSystemAAFontSettings=on'
_JAVA_OPTIONS+=' -Dswing.aatext=true'
#_JAVA_OPTIONS+=' -Dswing.defaultlaf=com.sun.java.swing.plaf.gtk.GTKLookAndFeel'
export _JAVA_OPTIONS
export _JAVA_AWT_WM_NONREPARENTING=1

export TMPDIR="/tmp/$USER"
[ -d "$TMPDIR" ] || mkdir -p -m 700 "$TMPDIR"

source ~/bin/ssh-agent.sh
source ~/bin/gpg-agent.sh
