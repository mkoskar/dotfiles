#!/usr/bin/env python
# -*- coding: utf-8 -*-
"""
    authinfo.py
    ~~~~~~~~~~~

    Reads ~/.authinfo.gpg to provide login credentials.

    :copyright: Copyright 2013 by Miroslav Koskar
    :license: BSD
"""

import argparse, os, re, sys
from os import path
from stat import S_IMODE


def get_authinfo_password(machine, login, port=None):
    srcfile = path.expanduser('~/.authinfo.gpg')
    fstat = os.stat(srcfile)

    uid = os.getuid()
    if not fstat.st_uid == uid:
        print("Warning: {0} is not owned by current process's user (uid={1})"
              .format(srcfile, uid), file=sys.stderr)

    expected_mode = 0o600
    if not S_IMODE(fstat.st_mode) == expected_mode:
        print('Warning: {0} should have mode {1}' \
              .format(srcfile, oct(expected_mode)[2:]), file=sys.stderr)

    authinfo = os.popen('gpg -q --no-tty -d ' + srcfile).read()

    if port:
        p = re.compile('^machine {0} login {1} password (.*) port {2}$' \
                       .format(re.escape(machine), re.escape(login), re.escape(port)),
                       re.M)
        res = p.search(authinfo)
        if res:
            return res.group(1)

    p = re.compile('^machine {0} login {1} password (.*)$' \
                   .format(re.escape(machine), re.escape(login)), re.M)
    res = p.search(authinfo)
    return res.group(1) if res else None

if __name__ == '__main__':
    parser = argparse.ArgumentParser(
        description='Reads ~/.authinfo.gpg to provide login credentials.'
    )
    parser.add_argument('machine')
    parser.add_argument('login')
    parser.add_argument('port', nargs='?')
    args = parser.parse_args()

    res = get_authinfo_password(args.machine, args.login, args.port)
    if res:
        print(res)
    else:
        sys.exit(1)
