Homefiles
=========

:Description: My home files
:License: BSD 2-Clause, if not stated otherwise

This repository contains files I typically checkout into my ``$HOME`` directory.
It contains configuration files of my favourite programs and various utilities.


bin
---

`<bin>`_ contains various utilities I've developed over time.
Some of them are specific to my work-flow, but some rather generic enough
are listed below.

`apod <bin/apod>`_
    Prints APOD (Astronomy Picture of the Day) image URL.

`apod-get <bin/apod-get>`_, `apod-bg <bin/apod-bg>`_
    Example of how to download and set APOD as your desktop background.

`audio <bin/audio>`_
    Simple command-line interface for basic audio controls.

`authinfo <bin/authinfo>`_
    Reads ``~/.authinfo.gpg`` to provide login credentials.

`backlight-toggle <bin/backlight-toggle>`_
    Simple command-line interface for 3 state backlight control.

`chp <bin/chp>`_
    Enables one to run and manipulate multiple Google Chrome / Chromium profiles
    at the same time.

`confirm <bin/confirm>`_
    Simple confirmation dialog.

`d <bin/d>`_, `d0 <bin/d0>`_
    Diff wrapper to produce nice Git-like formated diff output.

`dirsum <bin/dirsum>`_, `dirsum0 <bin/dirsum0>`_
    Make MD5 hash of a directory content and its metadata reliably.

`extnorm <bin/extnorm>`_
    Normalizes filename extensions by making them lowercase.

`ffp <bin/ffp>`_
    Enables one to run and manipulate multiple Firefox profiles at the same time.

`grep-nonascii <bin/grep-nonascii>`_
    Filter out lines containing non-ASCII characters.

`pg <bin/pg>`_, `pg0 <bin/pg0>`_, `pgx <bin/pgx>`_, `manpg <bin/manpg>`_
    Wrapper script to use Vim as a man(1) pager.

`mounted <bin/mounted>`_
    Is device mounted on mountpoint of some type with a specific mount option?

`mtp <bin/mtp>`_
    Simple ``jmtpfs`` frontend.

`osd <bin/osd>`_, `osdd <bin/osdd>`_
    OSD based on improved version of ``aosd_cat``.

`pacinfo <bin/pacinfo>`_
    Pretty pacman status report.

`pacman-altered <bin/pacman-altered>`_, `pacman-altered-job <bin/pacman-altered-job>`_
    Run this daily and receive report of what files has changed.

`pacman-disowned <bin/pacman-disowned>`_, `pacman-disowned-job <bin/pacman-disowned-job>`_
    Run this daily and receive report of what files has became "untracked".

`pgrepallx <bin/pgrepallx>`_
    ``pgrep -x`` filtered for current ``$DISPLAY``.

`rep <bin/rep>`_, `respawn <bin/respawn>`_
    Execute ``cmd [arg]...`` repeatedly.

`reqd <bin/reqd>`_
    Simple HTTP server that replies with request's headers in response body.

`scan <bin/scan>`_
    Simple SANE ``scanimage`` frontend.

`scroller <bin/scroller>`_
    Scrolls through passed arguments.

`sstat <bin/sstat>`_
    Pretty print socket info (from ``ss`` and ``netstat``).

`status <bin/status>`_
    Print status of: audio, backlight, bluetooth, dpms, wifi, and xkb.

`stdiner <bin/stdiner>`_
    Convert STDIN to positional command argument.

`tcolors-sel <bin/tcolors-sel>`_
    Scrolls through argument list (or predefined one if there are no arguments)
    and process each item by ``tcolors x``, hence changing ANSI terminal colors.

`terminfo <bin/terminfo>`_
    Print various information about current terminal.

`tfont <bin/tfont>`_
    Set passed font or scroll through predefined list.

`umountall <bin/umountall>`_
    Recursively umount and disassemble a device chain.

`xcmenuc <bin/xcmenuc>`_
    Simple ``xcmenu`` frontend.

`xfocusinfo <bin/xfocusinfo>`_
    Determines and prints out (in shell eval format) focused X screen,
    Xinerama head, root window id, and target window id.

`xserverq <bin/xserverq>`_
    Get information about X server (based on DISPLAY environment variable).
