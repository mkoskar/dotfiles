XKB
===

See also:

* https://www.x.org/releases/current/doc/kbproto/xkbproto.html
* https://xkbcommon.org/doc/current/keymap-text-format-v1.html

::

   # Lenovo T530:
   #
   #   MUTE    keycode 121     (keysym 0x1008ff12, XF86AudioMute)
   #   VOLDN   keycode 122     (keysym 0x1008ff11, XF86AudioLowerVolume)
   #   VOLUP   keycode 123     (keysym 0x1008ff13, XF86AudioRaiseVolume)
   #   MIC     keycode 198     (keysym 0x1008ffb2, XF86AudioMicMute)
   #   PROG1   keycode 156     (keysym 0x1008ff41, XF86Launch1)
   #
   #   Fn+F1   n/a
   #   Fn+F2   n/a
   #   Fn+F3   keycode 160     (keysym 0x1008ff2d, XF86ScreenSaver)
   #   Fn+F4   keycode 150     (keysym 0x1008ff2f, XF86Sleep)
   #   Fn+F5   keycode 246     (keysym 0x1008ff95, XF86WLAN)
   #   Fn+F6   keycode 220     (keysym 0x1008ff8f, XF86WebCam)
   #   Fn+F7   keycode 235     (keysym 0x1008ff59, XF86Display)
   #   Fn+F8   keycode 232     (keysym 0x1008ff03, XF86MonBrightnessDown)
   #   Fn+F9   keycode 233     (keysym 0x1008ff02, XF86MonBrightnessUp)
   #   Fn+F10  keycode 173     (keysym 0x1008ff16, XF86AudioPrev)
   #   Fn+F11  keycode 172     (keysym 0x1008ff14, XF86AudioPlay)
   #   Fn+F12  keycode 171     (keysym 0x1008ff17, XF86AudioNext)
   #
   #   Fn+1    keycode 248
   #   Fn+2    keycode 248
   #   Fn+B    keycode 127 + Control_L (keysym 0xff6b, Break)
   #   Fn+K    keycode 78              (keysym 0xff14, Scroll_Lock)
   #   Fn+P    keycode 127             (keysym 0xff13, Pause)
   #   Fn+S    keycode 107 + Alt_L     (keysym 0xff15, Sys_Req)
