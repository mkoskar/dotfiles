# ~/.profile
# - executed by the command interpreter for login shells
# - not read by bash(1), if ~/.bash_profile or ~/.bash_login exists

umask 022

export PATH="$HOME/.local/bin:$PATH"
export PATH="$HOME/bin:$PATH"

source ~/bin/ssh-agent.sh
source ~/bin/gpg-agent.sh

export LANG='en_US.UTF-8'
export LC_COLLATE='C'
export EDITOR='vim'
export BROWSER='firefox'
export LESS='-MRS#3'
export LESSHISTFILE='-'
export XAUTHORITY="$HOME/.Xauthority"
export VDPAU_DRIVER='va_gl'
export ORACLE_HOME='/opt/instantclient'

export JDK_HOME="$JAVA_HOME"
_JAVA_OPTIONS='-Dawt.useSystemAAFontSettings=on'
_JAVA_OPTIONS+=' -Dswing.aatext=true'
_JAVA_OPTIONS+=' -Dswing.defaultlaf=com.sun.java.swing.plaf.gtk.GTKLookAndFeel'
export _JAVA_OPTIONS
export _JAVA_AWT_WM_NONREPARENTING=1
