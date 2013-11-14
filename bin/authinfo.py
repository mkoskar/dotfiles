#!/usr/bin/env python

import os, re, sys


def get_authinfo_password(machine, login, port=None):
    authinfo = os.popen('gpg -q --no-tty -d ~/.authinfo.gpg').read()

    if port:
        p = re.compile('^machine %s login %s password ([^\\s]*) port %s$'
                       % (machine, login, port), re.M)
        res = p.search(authinfo)
        if res:
            return res.group(1)

    p = re.compile('^machine %s login %s password ([^\\s]*)$'
                   % (machine, login), re.M)
    res = p.search(authinfo)
    return res.group(1) if res else None

if __name__ == '__main__':
    res = get_authinfo_password(*sys.argv[1:])
    if res:
        print(res)
    else:
        sys.exit(1)
