# -*- coding: utf-8 -*-
# vim: fdm=marker

from subprocess import Popen, PIPE
import functools
import re

import weechat as w

SCRIPT_NAME = 'custom'
SCRIPT_AUTHOR = 'Miroslav Koškár <http://mkoskar.com/>'
SCRIPT_VERSION = '0.1'
SCRIPT_LICENSE = 'Apache 2.0'
SCRIPT_DESC = 'Personal customizations'

# Global {{{
# ----------------------------------------


def cmd(command, buffer='', mute=True):
    w.command(buffer, ('/mute ' if mute else '/') + command)


w.register(
    SCRIPT_NAME,
    SCRIPT_AUTHOR,
    SCRIPT_VERSION,
    SCRIPT_LICENSE,
    SCRIPT_DESC,
    '',
    '',
)

hd_buf = w.hdata_get('buffer')
hd_layout = w.hdata_get('layout')
hd_ldata = w.hdata_get('line_data')
hd_line = w.hdata_get('line')
hd_lines = w.hdata_get('lines')
hd_scroll = w.hdata_get('window_scroll')
hd_server = w.hdata_get('irc_server')
hd_win = w.hdata_get('window')

# }}}

# Buffers {{{
# ----------------------------------------

MERGE_RULES = [
    r'^irc\.\|*freenode([^.]*)\.#(bash|zsh)$',
    r'^irc\.\|*freenode([^.]*)\.#archlinux(-offtopic)?$',
]

timer_sort_merged = None


def buffers_iter():
    item = w.hdata_get_list(hd_buf, 'gui_buffers')
    while item:
        yield item
        item = w.hdata_pointer(hd_buf, item, 'next_buffer')


def merge(buffer):
    merge_group = None
    bname = w.buffer_get_string(buffer, 'full_name')
    for idx, rule in enumerate(MERGE_RULES):
        if re.match(rule, bname):
            w.buffer_set(buffer, 'localvar_set_merge_group', str(idx))
            merge_group = str(idx)
            break
    if not merge_group:
        w.buffer_set(buffer, 'localvar_del_merge_group', '')
        return
    for _buffer in buffers_iter():
        _merge_group = w.buffer_get_string(_buffer, 'localvar_merge_group')
        if buffer == _buffer:
            continue
        if not _merge_group:
            continue
        if merge_group == _merge_group:
            w.buffer_merge(_buffer, buffer)
            break


def sort_merged():
    buffers_by_number = {}

    for buffer in buffers_iter():
        bnum = w.buffer_get_integer(buffer, 'number')
        buffers = buffers_by_number.get(bnum, [])
        buffers.append(buffer)
        buffers_by_number[bnum] = buffers

    def sort_key(buffer):
        bname = w.buffer_get_string(buffer, 'full_name')
        key = [w.buffer_get_string(buffer, 'short_name')]
        key.extend(reversed(bname.split('.')))
        return key

    def merge(a, b):
        w.buffer_unmerge(b, 0)
        w.buffer_merge(b, a)
        return b

    for bnum, buffers in buffers_by_number.items():
        if len(buffers) > 1:
            buffers.sort(key=sort_key)
            active = 0
            for idx, buffer in enumerate(buffers):
                if w.buffer_get_integer(buffer, 'active') == 1:
                    active = idx
            functools.reduce(merge, buffers)
            cmd('input switch_active_buffer', buffers[active - 1])


def cb_timer_sort_merged(data, remaining_calls):
    global timer_sort_merged
    sort_merged()
    timer_sort_merged = None
    return w.WEECHAT_RC_OK


def buffer_init(buffer):
    bname = w.buffer_get_string(buffer, 'full_name')
    bits = bname.split('.')

    match = re.match(r'^irc\.\|*freenode([^.]*)\.#archlinux($|-.*)$', bname)
    if match:
        name = '#arch' + match.group(2)
        if name == '#arch-offtopic':
            name = '#arch-ot'
        w.buffer_set(buffer, 'short_name', name)

    if re.match(r'irc\.\|*bitlbee\.#twitter_mkoskar', bname):
        w.buffer_set(buffer, 'short_name', '#twitter')
        w.buffer_set(buffer, 'highlight_words', '@mkoskar')
    elif re.match(r'irc\.\|*gitter\.#neovim/neovim', bname):
        w.buffer_set(buffer, 'short_name', '#neovim')

    w.buffer_set(buffer, 'time_for_each_line', '0')
    if bname == 'perl.highmon':
        w.buffer_set(buffer, 'time_for_each_line', '1')

    btype = w.buffer_get_string(buffer, 'localvar_type')
    if btype == 'server':
        w.buffer_set(buffer, 'short_name', '@' + bits[2])
    elif btype == 'private':
        w.buffer_set(buffer, 'short_name', '+' + bits[2])
    elif btype == 'channel':
        nicklist = int(re.match(r'^irc\.\|*bitlbee\.&', bname) is not None)
        w.buffer_set(buffer, 'nicklist', str(nicklist))

    merge(buffer)
    global timer_sort_merged
    if not timer_sort_merged:
        timer_sort_merged = w.hook_timer(5, 0, 1, 'cb_timer_sort_merged', '')


def cb_signal_buffer_opened(data, signal, buffer):
    buffer_init(buffer)
    return w.WEECHAT_RC_OK


w.hook_signal('buffer_opened', 'cb_signal_buffer_opened', '')
w.hook_signal('irc_channel_opened', 'cb_signal_buffer_opened', '')
w.hook_signal('irc_pv_opened', 'cb_signal_buffer_opened', '')
w.hook_signal('irc_server_opened', 'cb_signal_buffer_opened', '')

buffers = list(buffers_iter())
for buffer in buffers:
    buffer_init(buffer)

# }}}

# Layouts {{{
# ----------------------------------------


def layouts_iter():
    item = w.hdata_get_list(hd_layout, 'gui_layouts')
    while item:
        yield item
        item = w.hdata_pointer(hd_layout, item, 'next_layout')


def layouts_name_iter():
    for layout in layouts_iter():
        yield w.hdata_string(hd_layout, layout, 'name')


def layout_find(name):
    return next((n for n in layouts_name_iter() if n == name), None)


def layout_current():
    return w.hdata_get_list(hd_layout, 'gui_layout_current')


def layout_current_name():
    layout_cur = layout_current()
    if layout_cur:
        return w.hdata_string(hd_layout, layout_cur, 'name')
    return None


def cb_command_layout_reset(data, buffer, args):
    if not args:
        cmd('layout apply windows')
        return w.WEECHAT_RC_OK

    if args == 'base':
        cmd('window merge all')
        cmd('window splith 15')
        cmd('buffer perl.highmon')
        cmd('window down')

    if args == 'horiz':
        cmd('window merge all')
        cmd('window splith 15')
        cmd('buffer perl.highmon')
        cmd('window down')
        cmd('window splith')
        cmd('window down')

    if args == 'vert':
        cmd('window merge all')
        cmd('window splith 15')
        cmd('buffer perl.highmon')
        cmd('window down')
        cmd('window splitv')

    elif args == 'core':
        cmd('window merge all')
        cmd('window splith 15')
        cmd('buffer perl.highmon')
        cmd('window down')
        cmd('buffer core.weechat')

    elif args == 'bitlbee':
        cmd('window merge all')
        cmd('window splith 15')
        cmd('buffer perl.highmon')
        cmd('window down')
        cmd('buffer bitlbee.&bitlbee')

    return w.WEECHAT_RC_OK


w.hook_command('layout_reset', '', '', '', '', 'cb_command_layout_reset', '')

keys = {
    'meta- ': 'layout_reset',
    'meta-;meta-1': 'layout_reset core',
    'meta-;meta-2': 'layout_reset bitlbee',
    'meta-;meta-3': 'layout_reset base',
    'meta-;meta-4': 'layout_reset horiz',
    'meta-;meta-5': 'layout_reset vert',
}

w.key_bind('default', keys)

# }}}

# Tabs {{{
# ----------------------------------------

tab_cur = None


def maybe_tab(src):
    try:
        tab = int(src)
    except (TypeError, ValueError):
        return None
    if not 0 < tab < 10:
        return None
    return tab


def tabs_all():
    tabs = []
    for name in layouts_name_iter():
        tab = maybe_tab(name)
        if tab is not None:
            tabs.append(tab)
    tabs.sort()
    return tabs


def cb_command_tab_go(data, buffer, args):
    global tab_cur
    tab_dst = maybe_tab(args)
    if tab_dst is None:
        return w.WEECHAT_RC_ERROR
    cmd('layout apply _zoom windows')
    cmd('layout del _zoom')
    if tab_cur is not None and layout_find(str(tab_cur)):
        cmd('layout store %s windows' % tab_cur)
    if layout_find(str(tab_dst)):
        cmd('layout apply %s windows' % tab_dst)
    else:
        cmd('layout store %s windows' % tab_dst)
    tab_cur = tab_dst
    w.bar_item_update('tabs')
    return w.WEECHAT_RC_OK


def cb_command_tab_next(data, buffer, args):
    global tab_cur
    norewind = args == '-norewind'
    tabs = tabs_all()
    if not len(tabs):
        tab_cur = None
        w.bar_item_update('tabs')
        return w.WEECHAT_RC_OK
    if tab_cur is None or len(tabs) == 1:
        tab_dst = tabs[0]
    else:
        tab_dst = next(
            (tab for tab in tabs if tab > tab_cur), tabs[-1 if norewind else 0]
        )
    cmd('tab_go %s' % tab_dst)
    return w.WEECHAT_RC_OK


def cb_command_tab_prev(data, buffer, args):
    global tab_cur
    norewind = args == '-norewind'
    tabs = tabs_all()
    if not len(tabs):
        tab_cur = None
        w.bar_item_update('tabs')
        return w.WEECHAT_RC_OK
    if tab_cur is None or len(tabs) == 1:
        tab_dst = tabs[0]
    else:
        tab_dst = next(
            (tab for tab in reversed(tabs)
             if tab < tab_cur), tabs[0 if norewind else -1]
        )
    cmd('tab_go %s' % tab_dst)
    return w.WEECHAT_RC_OK


def cb_command_tab_del(data, buffer, args):
    global tab_cur
    if args:
        target = maybe_tab(args)
    else:
        target = tab_cur
    if target is None:
        return w.WEECHAT_RC_ERROR
    cmd('layout del %s' % target)
    cmd('tab_prev -norewind')
    return w.WEECHAT_RC_OK


def cb_bar_item_tabs(data, item, window):
    tabs = tabs_all()
    return ' '.join(
        map(lambda tab: '[%s]' % tab if tab == tab_cur else str(tab), tabs)
    )


w.hook_command('tab_go', '', '', '', '', 'cb_command_tab_go', '')
w.hook_command('tab_next', '', '', '', '', 'cb_command_tab_next', '')
w.hook_command('tab_prev', '', '', '', '', 'cb_command_tab_prev', '')
w.hook_command('tab_del', '', '', '', '', 'cb_command_tab_del', '')
w.bar_item_new('tabs', 'cb_bar_item_tabs', '')

keys = {
    'meta-0': 'tab_del',
    'meta-l': 'tab_next',
    'meta-h': 'tab_prev',
}

for i in range(1, 10):
    keys['meta-%d' % i] = 'tab_go %d' % i

w.key_bind('default', keys)

# }}}

# Windows {{{
# ----------------------------------------


def windows_iter():
    item = w.hdata_get_list(hd_win, 'gui_windows')
    while item:
        yield item
        item = w.hdata_pointer(hd_win, item, 'next_window')


def windows_buffer_iter():
    for window in windows_iter():
        yield window, w.hdata_pointer(hd_win, window, 'buffer')


def cb_command_allwin_set_unread(data, buffer, args):
    for window, buffer in windows_buffer_iter():
        window_number = w.hdata_integer(hd_win, window, 'number')
        cmd('input set_unread_current_buffer', buffer)
        cmd('window scroll_bottom -window %s' % window_number)
    return w.WEECHAT_RC_OK


w.hook_command(
    'allwin_set_unread', '', '', '', '', 'cb_command_allwin_set_unread', ''
)

# }}}

# Other {{{
# ----------------------------------------


def servers_iter():
    item = w.hdata_get_list(hd_server, 'irc_servers')
    while item:
        yield item
        item = w.hdata_pointer(hd_server, item, 'next_server')


def server_opt(server_name, opt_name):
    opt = w.config_get('irc.server.%s.%s' % (server_name, opt_name))
    if w.config_option_is_null(opt):
        opt = w.config_get('irc.server_default.%s' % opt_name)
    return w.config_string(opt)


def connect_relay(server_name):
    cmd('server add |%s localhost/9000 -temp -ssl -nossl_verify '
        '-password=%s:${sec.data.relay}' % (server_name, server_name))
    cmd('connect |%s' % server_name)


def cb_command_connect_relay(data, buffer, args):
    if not args:
        for server in servers_iter():
            name = w.hdata_string(hd_server, server, 'name')
            if name.startswith('|'):
                continue
            if not w.config_string_to_boolean(server_opt(name, 'autoconnect')):
                continue
            connect_relay(name)
    else:
        connect_relay(args)
    return w.WEECHAT_RC_OK


def cb_command_grep_nick(data, buffer, args):
    bname = w.buffer_get_string(buffer, 'full_name')
    cmd('filter del grep_%s' % bname)
    if args:
        cmd('filter add grep_%s %s !nick_%s *' % (bname, bname, args))
    return w.WEECHAT_RC_OK


def cb_command_renick(data, buffer, args):
    nick_old = w.buffer_get_string(buffer, 'localvar_nick')
    if not nick_old:
        return w.WEECHAT_RC_ERROR
    nick_new = nick_old.rstrip('_')
    if nick_new == nick_old:
        return w.WEECHAT_RC_OK
    cmd('msg NickServ ghost %s' % nick_new)
    cmd('msg NickServ release %s' % nick_new)
    cmd('nick %s' % nick_new)
    return w.WEECHAT_RC_OK


def cb_command_urls(data, buffer, args):
    win = w.hdata_get_list(hd_win, 'gui_current_window')
    scroll = w.hdata_pointer(hd_win, win, 'scroll')
    lines = w.hdata_pointer(hd_buf, buffer, 'lines')
    line = w.hdata_pointer(hd_lines, lines, 'last_line')
    if scroll:
        after = w.hdata_integer(hd_scroll, scroll, 'lines_after')
        while after > 0 and line:
            ldata = w.hdata_pointer(hd_line, line, 'data')
            line = w.hdata_pointer(hd_line, line, 'prev_line')
            if w.hdata_char(hd_ldata, ldata, 'displayed'):
                after -= 1
    try:
        if data == 'open':
            p = Popen('tac | urls -o -r', shell=True, stdin=PIPE)
        elif data == 'yank':
            p = Popen('tac | urls -y -r', shell=True, stdin=PIPE)
        else:
            return w.WEECHAT_RC_ERROR
        count = 100
        while count > 0 and line:
            ldata = w.hdata_pointer(hd_line, line, 'data')
            if not w.hdata_char(hd_ldata, ldata, 'displayed'):
                line = w.hdata_pointer(hd_line, line, 'prev_line')
                continue
            lmsg = w.hdata_string(hd_ldata, ldata, 'message')
            p.stdin.write('%s\n' % w.string_remove_color(lmsg, ''))
            line = w.hdata_pointer(hd_line, line, 'prev_line')
            count -= 1
        p.stdin.close()
        p.wait()
    except Exception:
        return w.WEECHAT_RC_ERROR
    return w.WEECHAT_RC_OK


def cb_server_complete(data, name, buffer, completion):
    for server in servers_iter():
        name = w.hdata_string(hd_server, server, 'name')
        if name.startswith('|'):
            continue
        w.hook_completion_list_add(completion, name, 0, w.WEECHAT_LIST_POS_END)
    return w.WEECHAT_RC_OK


w.hook_command('connect_relay', '', '', '', '%(server_complete)', 'cb_command_connect_relay', '')
w.hook_command('grep_nick', '', '', '', '', 'cb_command_grep_nick', '')
w.hook_command('renick', '', '', '', '', 'cb_command_renick', '')
w.hook_command('urls_open', '', '', '', '', 'cb_command_urls', 'open')
w.hook_command('urls_yank', '', '', '', '', 'cb_command_urls', 'yank')

w.hook_completion('server_complete', '', 'cb_server_complete', '')

# }}}
