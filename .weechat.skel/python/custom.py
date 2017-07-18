# -*- coding: utf-8 -*-
# vim: fdm=marker

from subprocess import Popen, PIPE
import functools
import re

import weechat as w

SCRIPT_NAME = 'custom'
SCRIPT_AUTHOR = 'Miroslav Koskar <http://mkoskar.com/>'
SCRIPT_VERSION = '0.1'
SCRIPT_LICENSE = 'BSD 2-Clause'
SCRIPT_DESC = 'Personal customizations'


# Global {{{
# ----------------------------------------

class Expando(object):
    pass


def cmd(command, buffer='', mute=True):
    w.command(buffer, ('/mute ' if mute else '') + command)


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
hd_ldata = w.hdata_get('line_data')
hd_line = w.hdata_get('line')
hd_lines = w.hdata_get('lines')
hd_scroll = w.hdata_get('window_scroll')
hd_win = w.hdata_get('window')
hd_layout = w.hdata_get('layout')

# }}}


# Buffers {{{
# ----------------------------------------

MERGE_RULES = [
    r'^irc\.freenode\.#archlinux($|-.*)$',
]

timer_sort_merges = None


def buffers_iter():
    item = w.hdata_get_list(hd_buf, 'gui_buffers')
    while item:
        yield item
        item = w.hdata_pointer(hd_buf, item, 'next_buffer')


def sort_merges():
    buffers_by_number = {}

    for buffer in buffers_iter():
        bi = Expando()
        bi.buffer = buffer
        bi.number = w.buffer_get_integer(buffer, 'number')
        bi.full_name = w.buffer_get_string(buffer, 'full_name')
        bi.short_name = w.buffer_get_string(buffer, 'short_name')
        buffers = buffers_by_number.get(bi.number, [])
        buffers.append(bi)
        buffers_by_number[bi.number] = buffers

    def merge(a, b):
        w.buffer_unmerge(b.buffer, 0)
        w.buffer_merge(b.buffer, a.buffer)
        return b

    for number, buffers in buffers_by_number.iteritems():
        if len(buffers) > 1:
            buffers.sort(key=lambda bi: [bi.short_name, bi.full_name])
            functools.reduce(merge, buffers)
            cmd('/input switch_active_buffer', buffers[-1].buffer)


def sort_merges_lazy():
    global timer_sort_merges
    if timer_sort_merges:
        return
    timer_sort_merges = w.hook_timer(1, 0, 1, 'cb_timer_sort_merges', '')


def merge(buffer):
    bname = w.buffer_get_string(buffer, 'full_name')
    for rule in MERGE_RULES:
        if re.match(rule, bname):
            for _buffer in buffers_iter():
                _bname = w.buffer_get_string(_buffer, 'full_name')
                if bname != _bname and re.match(rule, _bname):
                    w.buffer_merge(buffer, _buffer)
                    break
            break


def buffer_init(buffer):
    bname = w.buffer_get_string(buffer, 'full_name')

    if bname == 'irc.bitlbee.#twitter_mkoskar':
        w.buffer_set(buffer, 'highlight_words', '@mkoskar')

    elif bname == 'irc.freenode.#archlinux':
        w.buffer_set(buffer, 'short_name', '#arch')

    elif bname == 'irc.freenode.#archlinux-offtopic':
        w.buffer_set(buffer, 'short_name', '#arch-ot')

    elif bname == 'irc.gitter.#neovim/neovim':
        w.buffer_set(buffer, 'short_name', '#neovim')

    w.buffer_set(buffer, 'nicklist', '0')

    if bname != 'perl.highmon':
        w.buffer_set(buffer, 'time_for_each_line', '0')

    merge(buffer)


def channel_init(buffer):
    bname = w.buffer_get_string(buffer, 'full_name')
    nicklist = int(re.match(r'^irc\.bitlbee\.&', bname) is not None)
    w.buffer_set(buffer, 'nicklist', str(nicklist))


def cb_timer_sort_merges(data, remaining_calls):
    global timer_sort_merges
    sort_merges()
    timer_sort_merges = None
    return w.WEECHAT_RC_OK


def cb_signal_buffer_opened(data, signal, buffer):
    buffer_init(buffer)
    return w.WEECHAT_RC_OK


def cb_signal_irc_channel_opened(data, signal, buffer):
    channel_init(buffer)
    return w.WEECHAT_RC_OK


w.hook_signal('buffer_opened', 'cb_signal_buffer_opened', '')
w.hook_signal('irc_channel_opened', 'cb_signal_irc_channel_opened', '')

for buffer in buffers_iter():
    buffer_init(buffer)


w.hook_command('dev1', '', '', '', '', 'cb_command_dev1', '')

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
    return next((_name for _name in layouts_name_iter() if _name == name), None)


def layout_current():
    return w.hdata_get_list(hd_layout, 'gui_layout_current')


def layout_current_name():
    layout_cur = layout_current()
    if layout_cur:
        return w.hdata_string(hd_layout, layout_cur, 'name')
    return None


def cb_command_layout_reset(data, buffer, args):
    if not args:
        cmd('/layout apply windows')
        return w.WEECHAT_RC_OK

    if args == 'base':
        cmd('/window merge all')
        cmd('/window splith 15')
        cmd('/buffer perl.highmon')
        cmd('/window down')

    if args == 'horiz':
        cmd('/window merge all')
        cmd('/window splith 15')
        cmd('/buffer perl.highmon')
        cmd('/window down')
        cmd('/window splith')
        cmd('/window down')

    if args == 'vert':
        cmd('/window merge all')
        cmd('/window splith 15')
        cmd('/buffer perl.highmon')
        cmd('/window down')
        cmd('/window splitv')

    elif args == 'core':
        cmd('/window merge all')
        cmd('/window splith 15')
        cmd('/buffer perl.highmon')
        cmd('/window down')
        cmd('/buffer core.weechat')

    elif args == 'bitlbee':
        cmd('/window merge all')
        cmd('/window splith 15')
        cmd('/buffer perl.highmon')
        cmd('/window down')
        cmd('/buffer bitlbee.&bitlbee')

    return w.WEECHAT_RC_OK


w.hook_command('layout_reset', '', '', '', '', 'cb_command_layout_reset', '')

keys = {
    'meta- ': '/layout_reset',
    'meta-;meta-1': '/layout_reset core',
    'meta-;meta-2': '/layout_reset bitlbee',
    'meta-;meta-3': '/layout_reset base',
    'meta-;meta-4': '/layout_reset horiz',
    'meta-;meta-5': '/layout_reset vert',
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
    cmd('/layout apply _zoom windows')
    cmd('/layout del _zoom')
    if tab_cur is not None and layout_find(str(tab_cur)):
        cmd('/layout store %s windows' % tab_cur)
    if layout_find(str(tab_dst)):
        cmd('/layout apply %s windows' % tab_dst)
    else:
        cmd('/layout store %s windows' % tab_dst)
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
        tab_dst = next((tab for tab in tabs if tab > tab_cur),
                       tabs[-1 if norewind else 0])
    cmd('/tab_go %s' % tab_dst)
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
        tab_dst = next((tab for tab in reversed(tabs) if tab < tab_cur),
                       tabs[0 if norewind else -1])
    cmd('/tab_go %s' % tab_dst)
    return w.WEECHAT_RC_OK


def cb_command_tab_del(data, buffer, args):
    global tab_cur
    if args:
        target = maybe_tab(args)
    else:
        target = tab_cur
    if target is None:
        return w.WEECHAT_RC_ERROR
    cmd('/layout del %s' % target)
    cmd('/tab_prev -norewind')
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
    'meta-0': '/tab_del',
    'meta-l': '/tab_next',
    'meta-h': '/tab_prev',
}

for i in range(1, 10):
    keys['meta-%d' % i] = '/tab_go %d' % i

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
        cmd('/input set_unread_current_buffer', buffer)
        cmd('/window scroll_bottom -window %s' % window_number)
    return w.WEECHAT_RC_OK


w.hook_command('allwin_set_unread', '', '', '', '', 'cb_command_allwin_set_unread', '')

# }}}


# Other {{{
# ----------------------------------------

def cb_command_grep_nick(data, buffer, args):
    bname = w.buffer_get_string(w.current_buffer(), 'full_name')
    cmd('/filter del grep_%s' % bname)
    if args:
        cmd('/filter add grep_%s %s !nick_%s *' % (bname, bname, args))
    return w.WEECHAT_RC_OK


def cb_command_urls_open(data, buffer, args):
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
        p = Popen('urls-open', stdin=PIPE)
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
    except Exception as e:
        return w.WEECHAT_RC_ERROR
    return w.WEECHAT_RC_OK


w.hook_command('grep_nick', '', '', '', '', 'cb_command_grep_nick', '')
w.hook_command('urls_open', '', '', '', '', 'cb_command_urls_open', '')

# }}}
