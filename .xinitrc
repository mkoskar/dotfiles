#!/bin/bash

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

# run all system xinitrc shell scripts
if [ -d /etc/X11/xinit/xinitrc.d ]; then
    for i in /etc/X11/xinit/xinitrc.d/* ; do
        if [ -x "$i" -a ! -d "$i" ]; then
            source "$i"
        fi
    done
fi

# XKB options
# - reset after keyboard is attached (eg. on wakeup from suspend)
# - set in the /etc/X11/xorg.conf.d/10-evdev.conf instead
#setxkbmap -option lv3:ralt_switch_multikey \
#          -option caps:ctrl_modifier \
#          -option misc:typo \
#          -option shift:both_capslock_cancel \
#          -option keypad:pointerkeys \
#          -option nbsp:zwnj2nb3zwj4

XSESSION="$1"
exec "$XSESSION"
