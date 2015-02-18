#!/usr/bin/env python
# -*- coding: utf-8 -*-
"""
    authinfo.py
    ~~~~~~~~~~~

    Reads ~/.authinfo.gpg to provide login credentials.

    :Compatibility: 2.7 / 3.2
    :Copyright: (c) 2013 Miroslav Koskar (http://mkoskar.com/)
    :License: BSD 2-Clause
"""

from __future__ import print_function

from os import getuid, path, stat
from stat import S_IMODE
from sys import stderr, exit
import argparse
import re
import subprocess


def get_authinfo_password(machine, login, port=None,
                          prefix=None, srcpath=None):
    if not prefix:
        prefix = ''

    if not srcpath:
        srcpath = '~/.authinfo.gpg'

    srcfile = path.expanduser(srcpath)
    fstat = stat(srcfile)

    uid = getuid()
    if not fstat.st_uid == uid:
        print("Warning: {0} is not owned by current process's user (uid={1})"
              .format(srcfile, uid), file=stderr)

    expected_mode = 0o600
    if not S_IMODE(fstat.st_mode) == expected_mode:
        print('Warning: {0} should have mode {1}' \
              .format(srcfile, oct(expected_mode)[2:]), file=stderr)

    authinfo = subprocess.check_output(['gpg', '-q', '--no-tty', '-d', srcfile],
                                       universal_newlines=True)

    if port:
        p = re.compile('^machine\s*{0}\s*login\s*{1}\s*password\s*(\S*)\s*port\s*{2}\s*$' \
                       .format(re.escape(machine), re.escape(login), re.escape(port)),
                       re.M)
        res = p.search(authinfo)
        if res:
            return prefix + res.group(1)

    p = re.compile('^machine\s*{0}\s*login\s*{1}\s*password\s*(\S*)\s*$' \
                   .format(re.escape(machine), re.escape(login)), re.M)
    res = p.search(authinfo)
    return prefix + res.group(1) if res else None


if __name__ == '__main__':
    parser = argparse.ArgumentParser(
        description='Reads ~/.authinfo.gpg to provide login credentials.'
    )
    parser.add_argument('--prefix', help='output prefix')
    parser.add_argument('--src', help='alternate authinfo file')
    parser.add_argument('machine')
    parser.add_argument('login')
    parser.add_argument('port', nargs='?')
    args = parser.parse_args()

    try:
        res = get_authinfo_password(args.machine, args.login, args.port,
                                    prefix=args.prefix, srcpath=args.src)
        if res:
            print(res)
        else:
            exit(1)
    except Exception as e:
        print('{}: error: {}'.format(parser.prog, e), file=stderr)
        exit(1)
