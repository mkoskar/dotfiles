#!/bin/bash

source ~/.profile

USERRESOURCES="$HOME/.Xresources"
USERMODMAP="$HOME/.Xmodmap"
SYSRESOURCES='/etc/X11/xinit/.Xresources'
SYSMODMAP='/etc/X11/xinit/.Xmodmap'

if [ -f "$SYSRESOURCES" ]; then
    xrdb -nocpp -merge "$SYSRESOURCES"
fi
if [ -f "$SYSMODMAP" ]; then
    xmodmap "$SYSMODMAP"
fi
if [ -f "$USERRESOURCES" ]; then
    xrdb -nocpp -merge "$USERRESOURCES"
fi
if [ -f "$USERMODMAP" ]; then
    xmodmap "$USERMODMAP"
fi

# run all system xinitrc shell scripts.
if [ -d '/etc/X11/xinit/xinitrc.d' ]; then
    for i in '/etc/X11/xinit/xinitrc.d/'* ; do
        if [ -x "$i" -a ! -d "$i" ]; then
            . "$i"
        fi
    done
fi

setxkbmap -option compose:ralt

XSESSION="$1"
exec "$XSESSION"
#exec ssh-agent gpg-agent --daemon "$XSESSION"
