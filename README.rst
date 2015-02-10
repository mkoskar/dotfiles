Homefiles
=========

:Description: My home files
:License: BSD 2-Clause, if not stated otherwise

This repository contains files I typically checkout to my ``$HOME`` directory.
It primarily contains configuration files for my favorite programs configured
to suit my taste. Whenever I stop using some program I move its configuration
to ``.attic``. Files that are expected to contain local configuration (connection
settings, passwords, etc.) are provided in example format (``*.example``).


bin
---

``bin`` on the other hand contains various utilities I've developed over time.
Some of them are very specific to my work-flow, but some are rather more
sophisticated or generic enough to be listed below.

``apod``
    Prints http://apod.nasa.gov/ image URL.

``apod-get`` and ``apod-bg``
    Example of how to download and set APOD (Astronomy Picture of the Day) as
    your desktop background.

``audio``
    Simple command-line interface for basic audio controls.

``authinfo``
    Reads ``~/.authinfo.gpg`` to provide login credentials.

``backlight-toggle``
    Simple command-line interface for 3 state backlight control.

``chp``
    Run and target multiple Google Chrome / Chromium profiles at the same time.

``confirm``
    Simple confirmation dialog.

``d``, ``d0``
    Diff wrapper to produce nice Git-like formated diff output.

``dirsum``, ``dirsum0``
    Make MD5 hash of a directory content and its metadata reliably.

``extnorm``
    Normalizes filename extensions by making them lowercase.

``ffp``
    Run and target multiple Firefox profiles at the same time.

``grep-nonascii``
    Filter out lines containing non-ASCII characters.

``pg``, ``pg0``, ``pgx``, ``manpg``
    Wrapper script to use Vim as a man(1) pager.

``mounted``
    Is device mounted on mountpoint of some type with a specific mount option?

``mtp``
    Simple ``jmtpfs`` frontend.

``osd``, ``osdd``
    OSD based on improved version of ``aosd_cat``

``pacinfo``
    Pretty pacman status report.

``pacman-altered``, ``pacman-altered-job``
    Run this daily and receive report of what files has changed.

``pacman-disowned``, ``pacman-disowned-job``
    Run this daily and receive report of what files has became "untracked".

``pgrepallx``
    ``pgrep -x`` filtered for current ``$DISPLAY``.

``rep``, ``respawn``
    Execute ``cmd [arg]...`` repeatedly.

``reqd``
    Simple HTTP server that replies with request's headers in response body.

``scan``
    Simple SANE ``scanimage`` frontend.

``scroller``
    Scrolls through passed arguments.

``sstat``
    Pretty print socket info (from ``ss`` and ``netstat``).

``status``
    Print status of: audio, backlight, bluetooth, dpms, wifi, and xkb.

``stdiner``
    Convert STDIN to positional command argument.

``tcolors-sel``
    Scrolls through argument list (or predefined one if there are no arguments)
    and process each item by ``tcolors x``, hence changing ANSI terminal colors.

``terminfo``
    Print various information about current terminal.

``tfont``
    Set passed font or scroll through predefined list.

``umountall``
    Recursively umount and disassemble a device chain.

``xcmenuc``
    Simple ``xcmenu`` frontend.

``xfocusinfo``
    Determines and prints out (in shell eval format) focused X screen,
    Xinerama head, root window id, and target window id.

``xserverq``
    Get information about X server (based on DISPLAY environment variable).
