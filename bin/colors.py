#!/usr/bin/env python
# -*- coding: utf-8 -*-
"""
    colors.py
    ~~~~~~~~~

    Set terminal ANSI colors (palette, foreground, background, cursor).
    Color can be given as name or RGB specification (e.g., #rrggbb).

    :Compatibility: Python 2.6 / 3.0
    :Copyright: Copyright 2013 by Miroslav Koskar
    :License: BSD
"""

from __future__ import print_function

from sys import argv, stdout
import os

_TERM = os.environ.get('TERM')
if os.environ.get('TMUX'):
    _seqfmt = '\033Ptmux;\033{}\a\033\\'
elif _TERM and (_TERM == 'screen' or _TERM.startswith('screen-')):
    _seqfmt = '\033P{}\a\033\\'
else:
    _seqfmt = '{}\033\\'


def _writeseq(seq, flush=False):
    stdout.write(_seqfmt.format(seq))
    if flush:
        stdout.flush()


def set_color(n, c, flush=False):
    _writeseq('\033]4;{};{}'.format(n, c), flush)


def set_colorfg(c, flush=False):
    _writeseq('\033]10;{}'.format(c), flush)


def set_colorbg(c, flush=False):
    _writeseq('\033]11;{}'.format(c), flush)


def set_colorcur(c, flush=False):
    _writeseq('\033]12;{}'.format(c), flush)


if __name__ == '__main__':
    import textwrap

    def err_usage():
        print(textwrap.dedent("""
            usage: colors {0..255} color
                   colors fg color
                   colors bg color
                   colors cur color

            Set terminal ANSI colors (palette, foreground, background, cursor).
            Color can be given as name or RGB specification (e.g., #rrggbb).
            """).strip()
        )
        exit(1)

    args = argv[1:]
    if len(args) != 2:
        err_usage()

    target = args[0]
    color = args[1]

    if target == 'fg':
        set_colorfg(color)
    elif target == 'bg':
        set_colorbg(color)
    elif target == 'cur':
        set_colorcur(color)
    else:
        try:
            set_color(int(target), color)
        except ValueError:
            err_usage()
