#!/usr/bin/env python
# -*- coding: utf-8 -*-
"""
    tcolors.py
    ~~~~~~~~~~

    Get/Set terminal ANSI colors.

    :Compatibility: Python 2.7 / 3.2
    :Copyright: Copyright 2013 by Miroslav Koskar
    :License: BSD
"""

from __future__ import print_function

from sys import stdin, stdout
import os
import re
import subprocess

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


def set_colorp(n, c, flush=False):
    _writeseq('\033]4;{};{}'.format(n, c), flush)


def get_colorp(n):
    # TODO
    pass


def set_colorfg(c, flush=False):
    _writeseq('\033]10;{}'.format(c), flush)


def get_colorfg():
    # TODO
    pass


def set_colorbg(c, flush=False):
    _writeseq('\033]11;{}'.format(c), flush)


def get_colorbg():
    # TODO
    pass


def set_colorcur(c, flush=False):
    _writeseq('\033]12;{}'.format(c), flush)


def get_colorcur():
    # TODO
    pass


def get_xcolors(file=None, prefix=None):
    proc = None
    if file:
        proc = subprocess.Popen(['cpp', '-P', file], stdout=subprocess.PIPE)
    else:
        proc = subprocess.Popen(['cpp', '-P'], stdin=stdin, stdout=subprocess.PIPE)
    xcolors = []
    xcolor_pattern = re.compile(re.escape(prefix) + r'\.?'
            '(color(\d+)|(foreground)|(background)|(cursorColor))\s*:(.*)$')
    while True:
        line = proc.stdout.readline()
        if not line:
            break
        line = line.decode().strip()
        match = xcolor_pattern.match(line)
        if not match:
            continue
        name, p, fg, bg, cur, value = match.groups()
        value = value.strip()
        xcolors.append((name, value, p, fg, bg, cur))
    if proc.wait() != 0:
        raise RuntimeError('Preprocessing failed!')
    return xcolors


def set_from_xcolors(file=None, prefix=None):
    xcolors = get_xcolors(file, prefix)
    for c in xcolors:
        name, value, p, fg, bg, cur = c
        if fg:
            set_colorfg(value)
        elif bg:
            set_colorbg(value)
        elif cur:
            set_colorcur(value)
        else:
            set_colorp(p, value)
    return xcolors


if __name__ == '__main__':
    import argparse
    import signal
    import textwrap

    signal.signal(signal.SIGINT, signal.SIG_DFL)

    parser = argparse.ArgumentParser(
        formatter_class=argparse.RawDescriptionHelpFormatter,
        description=textwrap.dedent("""
            Get/Set terminal ANSI colors.
            Color can be given as name or RGB specification (e.g., #rrggbb).
            """
        )
    )
    subparsers = parser.add_subparsers(dest='mode')
    p_parser = subparsers.add_parser(
        'p',
        help='get/set palette color',
        description='Get/Set palette color.'
    )
    f_parser = subparsers.add_parser(
        'f',
        help='get/set foreground color',
        description='Get/Set foreground color.'
    )
    b_parser = subparsers.add_parser(
        'b',
        help='get/set background color',
        description='Get/Set background color.'
    )
    c_parser = subparsers.add_parser(
        'c',
        help='get/set cursor color',
        description='Get/Set cursor color.'
    )
    x_parser = subparsers.add_parser(
        'x',
        help='set from X resources',
        description='Set colors from X resources.'
    )

    p_parser.add_argument('index', help='palette color index')
    p_parser.add_argument('color', nargs='?', help='palette color')
    f_parser.add_argument('color', nargs='?', help='foreground color')
    b_parser.add_argument('color', nargs='?', help='background color')
    c_parser.add_argument('color', nargs='?', help='cursor color')
    x_parser.add_argument('-p', '--print', action='store_true',
                          help="don't apply, print-out only")
    x_parser.add_argument('--prefix', default='*',
                          help='consider only X resources with PREFIX (default: *)')
    x_parser.add_argument('file', help="'-' for stdin")

    args = parser.parse_args()

    if args.mode == 'p':
        if args.color:
            set_colorp(args.index, args.color)
        else:
            get_colorp(args.index)
    elif args.mode == 'f':
        if args.color:
            set_colorfg(args.color)
        else:
            get_colorfg()
    elif args.mode == 'b':
        if args.color:
            set_colorbg(args.color)
        else:
            get_colorbg()
    elif args.mode == 'c':
        if args.color:
            set_colorcur(args.color)
        else:
            get_colorcur()
    elif args.mode == 'x':
        if args.print:
            for c in get_xcolors(args.file, args.prefix):
                print(c[0], c[1])
        else:
            set_from_xcolors(args.file, args.prefix)
    else:
        parser.print_usage()
