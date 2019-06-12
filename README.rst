dotfiles
========

:Description: My $HOME files
:License: Apache 2.0, if not stated otherwise

This repository contains files I typically checkout into my ``$HOME`` directory.
It contains configuration files of my favorite programs and various utilities.


bin
---

`<bin>`_ contains various utilities I've developed over time.
Some of them are specific to my work-flow, but some rather generic enough
are listed below.

`acpidecode <bin/acpidecode>`_
    Dump & decode all ACPI tables into your $PAGER.

`apod <bin/apod>`_
    Example of how to download and set APOD as your desktop background.

`apodurl <bin/apodurl>`_
    Prints `APOD <http://apod.nasa.gov/>`_ image URL.

`audio <bin/audio>`_
    Simple command-line interface for basic audio controls.

`authinfo.py <bin/authinfo.py>`_
    Reads ``~/.authinfo.gpg`` to provide login credentials.

`backlight <bin/system/backlight>`_
    Simple backlight controller.

`backlight-info <bin/backlight-info>`_
    Report on all backlight devices.

`bashx <bin/bashx>`_
    Bash extended for `common aliases and function <bin/shx.sh>`_.

`batcap <bin/batcap>`_, `batinfo <bin/batinfo>`_
    Battery summary.

`cam <bin/cam>`_
    Simple ``gphoto2`` frontend.

`chp <bin/chp>`_, `chpg <bin/chpg>`_, `chsocks <bin/chsocks>`_, `chtor <bin/chtor>`_
    Enables one to run and manipulate multiple Google Chrome / Chromium profiles
    at the same time.

`cmdline <bin/cmdline>`_
    Prints out (in shell eval format) cmdline or n-th argument of a process.

`confirm <bin/confirm>`_
    Simple confirmation dialog.

`ctlseq-wrap <bin/ctlseq-wrap>`_
    Make terminal control sequence to bypass ``tmux`` or ``screen``.

`d <bin/d>`_, `d0 <bin/d0>`_
    Diff wrapper to produce nice Git-like formated diff output.

`ddcload <bin/ddcload>`_
    Apply settings to all DDC monitors by profile.

`dict <bin/dict>`_
    Simple DICT (``dict.org``) client.

`dirsum <bin/dirsum>`_, `dirsum0 <bin/dirsum0>`_
    Make SHA1 digest of a directory content and its metadata reliably.

`dispcal-calibrate <bin/dispcal-calibrate>`_, `dispcal-clear <bin/dispcal-clear>`_, `dispcal-report <bin/dispcal-report>`_
    Monitor calibrating utilities.

`dpicalc <bin/dpicalc>`_, `dpiinfo <bin/dpiinfo>`_,
    Simple DPI calculator.

`edidsum <bin/edidsum>`_
    Compute SHA1 digest of EDIDs of all connected monitors.

`environ <bin/environ>`_
    Prints out environment of a process.

`extnorm <bin/extnorm>`_
    Normalizes filename extensions by making them lowercase.

`ffp <bin/ffp>`_, `ff <bin/ff>`_, `ffdev <bin/ffdev>`_
    Enables one to run and manipulate multiple Firefox profiles at the same time.

`fontsinfo <bin/fontsinfo>`_
    Report on matches of common font aliases and families.

`gitall <bin/gitall>`_
    Execute command on each git repository under CWD.

`github-repos <bin/github-repos>`_
    List public repositories for the specified user.

`grep-nonascii <bin/grep-nonascii>`_
    Filter out lines containing non-ASCII characters.

`hostsblock-fetch <bin/system/hostsblock-fetch>`_
    Fetch and process hostsblock lists.

`ipinfo <bin/ipinfo>`_, `ipinfo-tor <bin/ipinfo-tor>`_
    Simple "What is my IP" client.

`localeinfo <bin/localeinfo>`_
    Report on current locale.

`menuer <bin/menuer>`_
    Turn STDIN lines into parametrized menu.

`osd <bin/osd>`_, `osdd <bin/osdd>`_
    OSD based on improved version of ``aosd_cat``.

`pacblame <bin/pacblame>`_
    Blame installed packages by installed size, date, etc.

`pacinfo <bin/system/pacinfo>`_
    Pretty pacman status report.

`pacman-altered <bin/system/pacman-altered>`_, `pacman-altered-job <bin/system/pacman-altered-job>`_
    Run this daily and receive report of what files has changed.

`pacman-disowned <bin/system/pacman-disowned>`_, `pacman-disowned-job <bin/system/pacman-disowned-job>`_
    Run this daily and receive report of what files has became "untracked".

`pb <bin/pb>`_
    Simple pastebin (``ix``, ``ptpb``, ``sprunge``) frontend.

`pg <bin/pg>`_, `pg0 <bin/pg0>`_, `pgx <bin/pgx>`_, `manpg <bin/manpg>`_
    Wrapper script to use Vim as a pager.

`playctl <bin/playctl>`_
    Simple ``playerctl`` wrapper with bluetooth support.

`powersave <bin/system/powersave>`_
    Simple way to switch power saving on or off.

`rep <bin/rep>`_
    Execute ``cmd [arg]...`` repeatedly.

`reqd <bin/reqd>`_
    Simple HTTP server that replies with request's headers in response body.

`rfkill-state <bin/rfkill-state>`_
    Report or set rfkill states.

`scan <bin/scan>`_
    Simple SANE ``scanimage`` frontend.

`scast <bin/scast>`_
    Simple screencast utility.

`scroller <bin/scroller>`_
    Scrolls through passed arguments.

`selfie <bin/selfie>`_
    Simple selfie utility with a preview.

`sortb <bin/sortb>`_
    Sort whitespace lines separated blocks.

`srun <bin/srun>`_
    Execute ``cmd [arg]...`` as transient service inside user's service manager.

`sshgen-cert <bin/sshgen-cert>`_
    Generate SSH client or host certificate.

`sshot <bin/sshot>`_
    Simple screenshot utility.

`sstat <bin/system/sstat>`_
    Pretty print socket info (from ``ss`` and ``netstat``).

`status <bin/status>`_
    Print status of audio, backlight, bluetooth, dpms, wifi, etc.

`stdiner <bin/stdiner>`_
    Convert STDIN to positional command argument.

`striplns <bin/striplns>`_, `squashlns <bin/squashlns>`_
    Strip and squash whitespace lines.

`tcolors-sel <bin/tcolors-sel>`_
    Scrolls through argument list (or predefined one if there are no arguments)
    and process each item by ``tcolors x``, hence changing ANSI terminal colors.

`tcursor <bin/tcursor>`_
    Change terminal cursor style and color.

`terminfo <bin/terminfo>`_
    Print various information about current terminal.

`tfont <bin/tfont>`_
    Set passed font or scroll through predefined list.

`thermalinfo <bin/thermalinfo>`_
    Thermal sensors summary.

`tmux-all <bin/tmux-all>`_
    Execute command on each tmux's server socket.

`tmux-pipe <bin/tmux-pipe>`_
    Prints out tmux's pane content.

`umountall <bin/system/umountall>`_
    Recursively umount and disassemble a device chain.

`unicode <bin/unicode>`_
    Display various Unicode data.

`unraw <bin/unraw>`_
    Interpret raw terminal ouput using ``libvterm``.

`urlres <bin/urlres>`_
    Resolve URL redirects.

`urls <bin/urls>`_
    Open / Yank / Select an URL.

`urlsh <bin/urlsh>`_
    URL shortener.

`vtinfo <bin/system/vtinfo>`_
    Get some information about the active VT.

`vtswitch-lock <bin/system/vtswitch-lock>`_
    Locks (or unlocks) VT switching.

`x <bin/x>`_, `xx <bin/xx>`_
    X session startup script (for X and Xephyr respectively).

`lsmon <bin/lsmon>`_
    Traverses and prints out information about active monitors.

`xserverq <bin/xserverq>`_
    Get information about X server (based on DISPLAY environment variable).

`yt2m3u <bin/yt2m3u>`_
    Convert YouTube URLs to M3U playlist.
