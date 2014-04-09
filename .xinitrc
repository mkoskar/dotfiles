#!/bin/bash

if [ -d /etc/X11/xinit/xinitrc.d ]; then
    for i in /etc/X11/xinit/xinitrc.d/* ; do
        [ -f "$i" ] && source "$i"
    done
fi

xresources=~/.Xresources
if [ -f "$xresources" ]; then
    xrdb -merge "$xresources"
fi

xmodmap=~/.Xmodmap
if [ -f "$xmodmap" ]; then
    xmodmap "$xmodmap"
fi

# XKB options:
# * reset after keyboard is re-attached (e.g., on wakeup from suspend)
# * duplicated in /etc/X11/xorg.conf.d/10-evdev.conf

setxkbmap -option lv3:ralt_switch_multikey \
          -option caps:ctrl_modifier \
          -option shift:both_capslock_cancel \
          -option keypad:pointerkeys \
          -option nbsp:zwnj2nb3zwj4

# See:
# * /usr/share/X11/xkb/rules/xorg.lst
# * /usr/share/X11/xkb/symbols/typo
# * /usr/share/X11/locale/compose.dir
# * /usr/share/X11/locale/en_US.UTF-8/Compose
#
# lv3:ralt_switch_multikey      Right Alt (3rd level), Shift+Right Alt key is Compose.
# caps:ctrl_modifier            Make Caps Lock an additional Ctrl.
# misc:typo                     Enable extra typographic characters (symbols/typo).
# shift:both_capslock_cancel    Both Shift keys together activate Caps Lock,
#                               one Shift key deactivates.
# keypad:pointerkeys            Shift + NumLock toggles PointerKeys.
# nbsp:zwnj2nb3zwj4             Zero-width non-joiner character at second level,
#                               non-breakable space character at third level,
#                               zero-width joiner at fourth level.

# US keyboard layout
setxkbmap us cz_sk_de

[ $# -gt 0 ] && exec "$@"
