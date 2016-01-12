# -*- coding: utf-8 -*-
# vim: fdm=marker

SCRIPT_NAME = 'custom'
SCRIPT_AUTHOR = 'Miroslav Koskar <http://mkoskar.com/>'
SCRIPT_VERSION = '0.1'
SCRIPT_LICENSE = 'BSD 2-Clause'
SCRIPT_DESC = 'Personal customizations'


# Global {{{
# ----------------------------------------

import weechat

from collections import namedtuple
import re


class Expando(object):
    pass


def cmd(command, buffer='', mute=True):
    weechat.command(buffer, ('/mute ' if mute else '') + command)


weechat.register(
    SCRIPT_NAME,
    SCRIPT_AUTHOR,
    SCRIPT_VERSION,
    SCRIPT_LICENSE,
    SCRIPT_DESC,
    '',
    '',
)

# }}}


# Buffers {{{
# ----------------------------------------

MERGE_RULES = [
    r'^irc\.bitlbee\.&',
    r'^irc\.freenode\.#archlinux($|-.*)$',
]

timer_sort_merges = None


def buffers_iter():
    hdata = weechat.hdata_get('buffer')
    item = weechat.hdata_get_list(hdata, 'gui_buffers')
    while item:
        yield item
        item = weechat.hdata_pointer(hdata, item, 'next_buffer')


def buffers_visited_iter():
    hdata = weechat.hdata_get('buffer_visited')
    item = weechat.hdata_get_list(hdata, 'gui_buffers_visited')
    while item:
        yield weechat.hdata_pointer(hdata, item, 'buffer')
        item = weechat.hdata_pointer(hdata, item, 'next_buffer')


def sort_merges():
    buffers_by_number = {}

    for buffer in buffers_iter():
        bi = Expando()
        bi.buffer = buffer
        bi.number = weechat.buffer_get_integer(buffer, 'number')
        bi.full_name = weechat.buffer_get_string(buffer, 'full_name')
        bi.short_name = weechat.buffer_get_string(buffer, 'short_name')
        buffers = buffers_by_number.get(bi.number, [])
        buffers.append(bi)
        buffers_by_number[bi.number] = buffers

    def merge(a, b):
        weechat.buffer_unmerge(b.buffer, 0)
        weechat.buffer_merge(b.buffer, a.buffer)
        return b

    for number, buffers in buffers_by_number.iteritems():
        if len(buffers) > 1:
            buffers.sort(key=lambda bi: [bi.short_name, bi.full_name])
            reduce(merge, buffers)
            cmd('/input switch_active_buffer', buffers[-1].buffer)


def sort_merges_lazy():
    global timer_sort_merges
    if timer_sort_merges:
        return
    timer_sort_merges = weechat.hook_timer(1, 0, 1, 'cb_timer_sort_merges', '')


def cb_timer_sort_merges(data, remaining_calls):
    global timer_sort_merges
    sort_merges()
    timer_sort_merges = None
    return weechat.WEECHAT_RC_OK


def merge(buffer):
    full_name = weechat.buffer_get_string(buffer, 'full_name')
    for rule in MERGE_RULES:
        if re.match(rule, full_name):
            for _buffer in buffers_iter():
                _full_name = weechat.buffer_get_string(_buffer, 'full_name')
                if full_name != _full_name and re.match(rule, _full_name):
                    weechat.buffer_merge(buffer, _buffer)
                    break
            break


def cb_signal_buffer_merged(data, signal, buffer):
    sort_merges_lazy()
    return weechat.WEECHAT_RC_OK


def cb_signal_buffer_opened(data, signal, buffer):
    full_name = weechat.buffer_get_string(buffer, 'full_name')

    if full_name == 'irc.bitlbee.#twitter':
        weechat.buffer_set(buffer, 'highlight_words', '@mkoskar')

    elif full_name == 'irc.freenode.#archlinux':
        weechat.buffer_set(buffer, 'short_name', '#arch')

    elif full_name == 'irc.freenode.#archlinux-offtopic':
        weechat.buffer_set(buffer, 'short_name', '#arch-ot')

    elif full_name == 'irc.gitter.#neovim/neovim':
        weechat.buffer_set(buffer, 'short_name', '#neovim')

    nicklist_local = int(re.match(r'^irc\.bitlbee\.&', full_name) is not None)
    weechat.buffer_set(buffer, 'localvar_set_nicklist_local', str(nicklist_local))

    merge(buffer)
    return weechat.WEECHAT_RC_OK


weechat.hook_signal('buffer_merged', 'cb_signal_buffer_merged', '')
weechat.hook_signal('buffer_opened', 'cb_signal_buffer_opened', '')

# }}}


# Layouts {{{
# ----------------------------------------


def layouts_iter():
    hdata = weechat.hdata_get('layout')
    item = weechat.hdata_get_list(hdata, 'gui_layouts')
    while item:
        yield item
        item = weechat.hdata_pointer(hdata, item, 'next_layout')


def layouts_name_iter():
    hdata = weechat.hdata_get('layout')
    for layout in layouts_iter():
        yield weechat.hdata_string(hdata, layout, 'name')


def layout_find(name):
    return next((_name for _name in layouts_name_iter() if _name == name), None)


def layout_current():
    hdata = weechat.hdata_get('layout')
    return weechat.hdata_get_list(hdata, 'gui_layout_current')


def layout_current_name():
    hdata = weechat.hdata_get('layout')
    layout_cur = layout_current()
    if layout_cur:
        return weechat.hdata_string(hdata, layout_cur, 'name')
    return None


def cb_command_layout_reset(data, buffer, args):
    if not args:
        cmd('/layout apply windows')
        return weechat.WEECHAT_RC_OK

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

    return weechat.WEECHAT_RC_OK


weechat.hook_command('layout_reset', '', '', '', '', 'cb_command_layout_reset', '')

keys = {
    'meta- ': '/layout_reset',
    'meta-;meta-1': '/layout_reset core',
    'meta-;meta-2': '/layout_reset bitlbee',
    'meta-;meta-3': '/layout_reset base',
    'meta-;meta-4': '/layout_reset horiz',
    'meta-;meta-5': '/layout_reset vert',
}

weechat.key_bind('default', keys)

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
        return weechat.WEECHAT_RC_ERROR
    cmd('/layout apply _zoom windows')
    cmd('/layout del _zoom')
    if tab_cur is not None and layout_find(str(tab_cur)):
        cmd('/layout store %s windows' % tab_cur)
    if layout_find(str(tab_dst)):
        cmd('/layout apply %s windows' % tab_dst)
    else:
        cmd('/layout store %s windows' % tab_dst)
    tab_cur = tab_dst
    weechat.bar_item_update('tabs')
    return weechat.WEECHAT_RC_OK


def cb_command_tab_next(data, buffer, args):
    global tab_cur
    norewind = args == '-norewind'
    tabs = tabs_all()
    if not len(tabs):
        tab_cur = None
        weechat.bar_item_update('tabs')
        return weechat.WEECHAT_RC_OK
    if tab_cur is None or len(tabs) == 1:
        tab_dst = tabs[0]
    else:
        tab_dst = next((tab for tab in tabs if tab > tab_cur),
            tabs[-1 if norewind else 0])
    cmd('/tab_go %s' % tab_dst)
    return weechat.WEECHAT_RC_OK


def cb_command_tab_prev(data, buffer, args):
    global tab_cur
    norewind = args == '-norewind'
    tabs = tabs_all()
    if not len(tabs):
        tab_cur = None
        weechat.bar_item_update('tabs')
        return weechat.WEECHAT_RC_OK
    if tab_cur is None or len(tabs) == 1:
        tab_dst = tabs[0]
    else:
        tab_dst = next((tab for tab in reversed(tabs) if tab < tab_cur),
            tabs[0 if norewind else -1])
    cmd('/tab_go %s' % tab_dst)
    return weechat.WEECHAT_RC_OK


def cb_command_tab_del(data, buffer, args):
    global tab_cur
    if args:
        target = maybe_tab(args)
    else:
        target = tab_cur
    if target is None:
        return weechat.WEECHAT_RC_ERROR
    cmd('/layout del %s' % target)
    cmd('/tab_prev -norewind')
    return weechat.WEECHAT_RC_OK


def cb_bar_item_tabs(data, item, window):
    tabs = tabs_all()
    return ' '.join(
        map(lambda tab: '[%s]' % tab if tab == tab_cur else str(tab), tabs)
    )


weechat.hook_command('tab_go', '', '', '', '', 'cb_command_tab_go', '')
weechat.hook_command('tab_next', '', '', '', '', 'cb_command_tab_next', '')
weechat.hook_command('tab_prev', '', '', '', '', 'cb_command_tab_prev', '')
weechat.hook_command('tab_del', '', '', '', '', 'cb_command_tab_del', '')
weechat.bar_item_new('tabs', 'cb_bar_item_tabs', '')

keys = {
    'meta-0': '/tab_del',
    'meta-l': '/tab_next',
    'meta-h': '/tab_prev',
}

for i in xrange(1, 10):
    keys['meta-%d' % i] = '/tab_go %d' % i

weechat.key_bind('default', keys)

# }}}


# Windows {{{
# ----------------------------------------


def windows_iter():
    hdata = weechat.hdata_get('window')
    item = weechat.hdata_get_list(hdata, 'gui_windows')
    while item:
        yield item
        item = weechat.hdata_pointer(hdata, item, 'next_window')


def windows_buffer_iter():
    hdata = weechat.hdata_get('window')
    for window in windows_iter():
        yield window, weechat.hdata_pointer(hdata, window, 'buffer')


def cb_command_allwin_set_unread(data, buffer, args):
    hdata = weechat.hdata_get('window')
    for window, buffer in windows_buffer_iter():
        window_number = weechat.hdata_integer(hdata, window, 'number')
        cmd('/input set_unread_current_buffer', buffer)
        cmd('/window scroll_bottom -window %s' % window_number)
    return weechat.WEECHAT_RC_OK


weechat.hook_command('allwin_set_unread', '', '', '', '', 'cb_command_allwin_set_unread', '')

# }}}


# Other {{{
# ----------------------------------------

def cb_command_grep_nick(data, buffer, args):
    buf = weechat.buffer_get_string(weechat.current_buffer(), 'full_name')
    cmd('/filter del grep_%s' % buf)
    if args:
        cmd('/filter add grep_%s %s !nick_%s *' % (buf, buf, args))
    return weechat.WEECHAT_RC_OK


def cb_command_nicklist_toggle(data, buffer, args):
    nicklist_local = int(weechat.buffer_get_string(buffer, 'localvar_nicklist_local') or 0)
    weechat.buffer_set(buffer, 'localvar_set_nicklist_local', str(int(not nicklist_local)))
    cmd('/window refresh')
    return weechat.WEECHAT_RC_OK


def cb_command_toggle_integer(data, buffer, args):
    option, a, b = args.split('!')
    a, b = int(a), int(b)
    cmd('/set %s %d' % (
        option,
        a if weechat.config_integer(weechat.config_get(option)) != a else b,
    ))
    return weechat.WEECHAT_RC_OK


def cb_command_toggle_string(data, buffer, args):
    option, a, b = args.split('!')
    cmd('/set %s %s' % (
        option,
        a if weechat.config_string(weechat.config_get(option)) != a else b,
    ))
    return weechat.WEECHAT_RC_OK


weechat.hook_command('grep_nick', '', '', '', '', 'cb_command_grep_nick', '')
weechat.hook_command('nicklist_toggle', '', '', '', '', 'cb_command_nicklist_toggle', '')
weechat.hook_command('toggle_integer', '', '', '', '', 'cb_command_toggle_integer', '')
weechat.hook_command('toggle_string', '', '', '', '', 'cb_command_toggle_string', '')

# }}}
