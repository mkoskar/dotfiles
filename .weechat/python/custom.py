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

w.register(
    SCRIPT_NAME,
    SCRIPT_AUTHOR,
    SCRIPT_VERSION,
    SCRIPT_LICENSE,
    SCRIPT_DESC,
    '',
    ''
)

hd_bar = w.hdata_get('bar')
hd_bar_win = w.hdata_get('bar_window')
hd_bar_item = w.hdata_get('bar_item')
hd_buf = w.hdata_get('buffer')
hd_layout = w.hdata_get('layout')
hd_line = w.hdata_get('line')
hd_line_data = w.hdata_get('line_data')
hd_lines = w.hdata_get('lines')
hd_server = w.hdata_get('irc_server')
hd_win = w.hdata_get('window')
hd_win_scroll = w.hdata_get('window_scroll')


def cmd(command, buffer='', mute=True):
    w.command(buffer, ('/mute ' if mute else '/') + command)


def key_bind(ctxt, key, val):
    cmd(f'/key bindctxt {ctxt} {key} {val}')


def cb_bar_item_cmd_btn(data, item, window, buffer, extra_info):
    item_name = w.hdata_string(hd_bar_item, item, 'name')
    extra_vars = {}
    if item_name == 'btn_win_zoom':
        extra_vars['zoom'] = '1' if layout_find('_zoom') else '0'
    return w.string_eval_expression(data, {
        'window': window, 'buffer': buffer}, extra_vars, {})


def cb_bar_item_cmd_btn_update(data, signal, buffer):
    w.bar_item_update(data)
    return w.WEECHAT_RC_OK


def bar_item_cmd_btn(name, label, cmd, update_on=[]):
    w.bar_item_new('(extra)' + name, 'cb_bar_item_cmd_btn', label)
    w.bar_item_update(name)
    key_bind('mouse', f'@item({name}):button1', cmd)
    for signal in update_on:
        w.hook_signal(signal, 'cb_bar_item_cmd_btn_update', name)


# }}}

# Buffers {{{
# ----------------------------------------

MERGE_RULES = [
    r'^irc\.freenode[^.]*\.#(bash|zsh)$',
    r'^irc\.freenode[^.]*\.#archlinux(-offtopic)?$',
]

timer_sort_merge = None
core = w.buffer_search_main()


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


def cb_timer_sort_merge(data, remaining_calls):
    global timer_sort_merge
    sort_merged()
    timer_sort_merge = None
    return w.WEECHAT_RC_OK


def buffer_init(buffer):
    bname = w.buffer_get_string(buffer, 'full_name')
    bname_ = bname.split('.')

    match = re.match(r'^irc\.freenode[^.]*\.#archlinux($|-.*)$', bname)
    if match:
        name = '#arch' + match.group(1)
        if name == '#arch-offtopic':
            name = '#arch-ot'
        w.buffer_set(buffer, 'short_name', name)

    if re.match(r'^irc\.bitlbee[^.]*\.#twitter_mkoskar$', bname):
        w.buffer_set(buffer, 'short_name', '#twitter')
        w.buffer_set(buffer, 'highlight_words', '@mkoskar')
    elif re.match(r'^irc\.gitter[^.]*\.#neovim/neovim$', bname):
        w.buffer_set(buffer, 'short_name', '#neovim')

    w.buffer_set(buffer, 'time_for_each_line', '0')
    if bname == 'perl.highmon':
        w.buffer_set(buffer, 'time_for_each_line', '1')

    btype = w.buffer_get_string(buffer, 'localvar_type')
    if btype == 'server':
        w.buffer_set(buffer, 'short_name', '@' + bname_[2])
    elif btype == 'private':
        w.buffer_set(buffer, 'short_name', '+' + bname_[2])
    elif btype == 'channel':
        nicklist = int(re.match(r'^irc\.bitlbee[^.]*\.&', bname) is not None)
        w.buffer_set(buffer, 'nicklist', str(nicklist))

    merge(buffer)
    global timer_sort_merge
    if not timer_sort_merge:
        timer_sort_merge = w.hook_timer(5, 0, 1, 'cb_timer_sort_merge', '')


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
    cmd('layout del _zoom')
    if not args:
        if tab_cur is not None:
            preset = w.buffer_get_string(
                core, f'localvar_tab{tab_cur}_preset'
            )
            if preset:
                cmd(f'layout_reset {preset}')
            else:
                cmd(f'layout apply tab{tab_cur}_saved windows')
                cmd(f'layout store tab{tab_cur} windows')
        return w.WEECHAT_RC_OK

    if args == '-':
        if tab_cur is not None:
            cmd(f'layout del tab{tab_cur}_saved')
            w.buffer_set(core, f'localvar_del_tab{tab_cur}_preset', '')
        return w.WEECHAT_RC_OK

    cmd('window merge all')
    cmd('window splith 20')
    cmd('buffer perl.highmon')
    cmd('window down')

    if args == 'core':
        cmd('buffer core.weechat')
    elif args == 'bitlbee':
        cmd('buffer &bitlbee')
    elif args == 'twitter':
        cmd('buffer #twitter')
    elif args:
        cmd(f'buffer {args}')

    if tab_cur is not None:
        cmd(f'layout store tab{tab_cur} windows')
        w.buffer_set(core, f'localvar_set_tab{tab_cur}_preset', args)
    return w.WEECHAT_RC_OK


def cb_command_layout_save(data, buffer, args):
    if tab_cur is not None:
        cmd(f'layout store tab{tab_cur}_saved windows')
        cmd(f'layout store tab{tab_cur} windows')
        w.buffer_set(core, f'localvar_del_tab{tab_cur}_preset', '')
    return w.WEECHAT_RC_OK


w.hook_command('layout_reset', '', '', '', '', 'cb_command_layout_reset', '')
w.hook_command('layout_save', '', '', '', '', 'cb_command_layout_save', '')

bar_item_cmd_btn('btn_layout_reset', 'RESET', '/layout_reset')
bar_item_cmd_btn('btn_layout_save', 'SAVE', '/layout_save')

key_bind('default', 'meta-space', '/layout_reset')
key_bind('default', 'meta-;meta-space', '/layout_save')
key_bind('default', 'meta-;meta-1', '/layout_reset core')
key_bind('default', 'meta-;meta-2', '/layout_reset bitlbee')
key_bind('default', 'meta-;meta-3', '/layout_reset twitter')

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
        match = re.match(r'^tab(\d+)$', name)
        if match:
            tab = maybe_tab(match.group(1))
            if tab is not None:
                tabs.append(tab)
    tabs.sort()
    return tabs


def cb_command_tab_del(data, buffer, args):
    global tab_cur
    if args:
        target = maybe_tab(args)
    else:
        target = tab_cur
    if target is None:
        return w.WEECHAT_RC_ERROR
    cmd(f'layout del tab{target}')
    cmd('tab_prev -norewind')
    return w.WEECHAT_RC_OK


def cb_command_tab_go(data, buffer, args):
    global tab_cur
    tab_dst = maybe_tab(args)
    if tab_dst is None:
        return w.WEECHAT_RC_ERROR
    cmd('layout apply _zoom windows')
    cmd('layout del _zoom')
    if tab_cur is not None and layout_find(f'tab{tab_cur}'):
        cmd(f'layout store tab{tab_cur} windows')
    tab_cur = tab_dst
    if layout_find(f'tab{tab_cur}'):
        cmd(f'layout apply tab{tab_cur} windows')
    else:
        cmd('layout_reset')
    bar_item_update_tabs()
    return w.WEECHAT_RC_OK


def cb_command_tab_next(data, buffer, args):
    global tab_cur
    norewind = args == '-norewind'
    tabs = tabs_all()
    if not len(tabs):
        tab_cur = None
        bar_item_update_tabs()
        return w.WEECHAT_RC_OK
    if tab_cur is None or len(tabs) == 1:
        tab_dst = tabs[0]
    else:
        tab_dst = next(
            (tab for tab in tabs if tab > tab_cur), tabs[-1 if norewind else 0]
        )
    cmd(f'tab_go {tab_dst}')
    return w.WEECHAT_RC_OK


def cb_command_tab_prev(data, buffer, args):
    global tab_cur
    norewind = args == '-norewind'
    tabs = tabs_all()
    if not len(tabs):
        tab_cur = None
        bar_item_update_tabs()
        return w.WEECHAT_RC_OK
    if tab_cur is None or len(tabs) == 1:
        tab_dst = tabs[0]
    else:
        tab_dst = next(
            (tab for tab in reversed(tabs)
             if tab < tab_cur), tabs[0 if norewind else -1]
        )
    cmd(f'tab_go {tab_dst}')
    return w.WEECHAT_RC_OK


w.hook_command('tab_del', '', '', '', '', 'cb_command_tab_del', '')
w.hook_command('tab_go', '', '', '', '', 'cb_command_tab_go', '')
w.hook_command('tab_next', '', '', '', '', 'cb_command_tab_next', '')
w.hook_command('tab_prev', '', '', '', '', 'cb_command_tab_prev', '')


def cb_bar_item_tab(data, item, window):
    tab = maybe_tab(data)
    if tab not in tabs_all():
        return ''
    return w.string_eval_expression(
        f'${{color:white,red}}[{tab}]${{color:reset}}'
        if tab == tab_cur else f'[{tab}]', {}, {}, {})


def cb_hsignal_tab_click(data, signal, hashtable):
    col = int(hashtable['_bar_item_col'])
    if col > 2:
        return w.WEECHAT_RC_OK
    tab = maybe_tab(data)
    if tab not in tabs_all():
        return w.WEECHAT_RC_OK
    cmd(f'tab_go {tab}')
    return w.WEECHAT_RC_OK


def bar_item_update_tabs():
    for i in range(1, 10):
        w.bar_item_update(f'tab{i}')


for i in range(1, 10):
    w.bar_item_new(f'tab{i}', 'cb_bar_item_tab', str(i))
    w.hook_hsignal(f'tab{i}_click', 'cb_hsignal_tab_click', str(i))
    key_bind('mouse', f'@item(tab{i}):button1', f'hsignal:tab{i}_click')
    key_bind('default', f'meta-{i}', f'/tab_go {i}')
    cmd(f'alias add {i} tab_go {i}')

bar_item_cmd_btn('btn_tab_del', 'TAB×', '/tab_del')
bar_item_cmd_btn('btn_tab_next', 'TAB+', '/tab_next')
bar_item_cmd_btn('btn_tab_prev', 'TAB-', '/tab_prev')

key_bind('default', 'meta-0', '/tab_del')
key_bind('default', 'meta-l', '/tab_next')
key_bind('default', 'meta-h', '/tab_prev')

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
        cmd(f'window scroll_bottom -window {window_number}')
    return w.WEECHAT_RC_OK


w.hook_command('allwin_set_unread', '', '', '', '',
               'cb_command_allwin_set_unread', '')

# }}}

# Other {{{
# ----------------------------------------


def servers_iter():
    item = w.hdata_get_list(hd_server, 'irc_servers')
    while item:
        yield item
        item = w.hdata_pointer(hd_server, item, 'next_server')


def server_opt(server_name, opt_name):
    opt = w.config_get(f'irc.server.{server_name}.{opt_name}')
    if w.config_option_is_null(opt):
        opt = w.config_get(f'irc.server_default.{opt_name}')
    return w.config_string(opt)


def connect_relay(server_name):
    cmd(f'server add {server_name}~ localhost/9000 -temp -ssl -nossl_verify '
        f'-password={server_name}:${{sec.data.relay}}')
    cmd(f'connect {server_name}~')


def cb_command_connect_relay(data, buffer, args):
    if not args:
        for server in servers_iter():
            name = w.hdata_string(hd_server, server, 'name')
            if name.endswith('~'):
                continue
            if not w.config_string_to_boolean(server_opt(name, 'autoconnect')):
                continue
            connect_relay(name)
    else:
        connect_relay(args)
    return w.WEECHAT_RC_OK


def cb_command_grep_nick(data, buffer, args):
    bname = w.buffer_get_string(buffer, 'full_name')
    cmd(f'filter del grep_{bname}')
    if args:
        cmd(f'filter add grep_{bname} {bname} !nick_{args} *')
    return w.WEECHAT_RC_OK


def cb_command_renick(data, buffer, args):
    nick_old = w.buffer_get_string(buffer, 'localvar_nick')
    if not nick_old:
        return w.WEECHAT_RC_ERROR
    nick_new = nick_old.rstrip('_')
    if nick_new == nick_old:
        return w.WEECHAT_RC_OK
    cmd(f'msg NickServ ghost {nick_new}')
    cmd(f'msg NickServ release {nick_new}')
    cmd(f'nick {nick_new}')
    return w.WEECHAT_RC_OK


def cb_command_urls(data, buffer, args):
    win = w.hdata_get_list(hd_win, 'gui_current_window')
    scroll = w.hdata_pointer(hd_win, win, 'scroll')
    lines = w.hdata_pointer(hd_buf, buffer, 'lines')
    line = w.hdata_pointer(hd_lines, lines, 'last_line')
    if scroll:
        after = w.hdata_integer(hd_win_scroll, scroll, 'lines_after')
        while after > 0 and line:
            ldata = w.hdata_pointer(hd_line, line, 'data')
            line = w.hdata_pointer(hd_line, line, 'prev_line')
            if w.hdata_char(hd_line_data, ldata, 'displayed'):
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
            if not w.hdata_char(hd_line_data, ldata, 'displayed'):
                line = w.hdata_pointer(hd_line, line, 'prev_line')
                continue
            lmsg = w.hdata_string(hd_line_data, ldata, 'message')
            p.stdin.write(f"{w.string_remove_color(lmsg, '')}")
            line = w.hdata_pointer(hd_line, line, 'prev_line')
            count -= 1
        p.stdin.close()
        p.wait()
    except Exception:
        return w.WEECHAT_RC_ERROR
    return w.WEECHAT_RC_OK


w.hook_command('connect_relay', '', '', '', '%(irc_servers)',
               'cb_command_connect_relay', '')
w.hook_command('grep_nick', '', '', '', '', 'cb_command_grep_nick', '')
w.hook_command('renick', '', '', '', '', 'cb_command_renick', '')
w.hook_command('urls_open', '', '', '', '', 'cb_command_urls', 'open')
w.hook_command('urls_yank', '', '', '', '', 'cb_command_urls', 'yank')


def cb_completion_irc_servers(data, name, buffer, completion):
    for server in servers_iter():
        name = w.hdata_string(hd_server, server, 'name')
        if name.endswith('~'):
            continue
        w.hook_completion_list_add(completion, name, 0, w.WEECHAT_LIST_POS_END)
    return w.WEECHAT_RC_OK


w.hook_completion('irc_servers', '', 'cb_completion_irc_servers', '')

bar_item_cmd_btn(
    'btn_filter',
    '[${if:${buffer.filter}?F:f}${if:${info:filters_enabled}?+:-}]',
    '/filter toggle @',
    ['buffer_switch', 'window_switch',
     'buffer_filters_enabled', 'buffer_filters_disabled',
     'filters_enabled', 'filters_disabled']
)

bar_item_cmd_btn(
    'btn_server',
    '${if:${type}==channel?@ ${server}}',
    '/server jump',
    ['buffer_switch', 'window_switch']
)

bar_item_cmd_btn(
    'btn_zoom',
    '${if:${buffer.mixed_lines}!=0x0?[${if:${buffer.zoomed}?Z:z}]}',
    '/input zoom_merged_buffer',
    ['buffer_switch', 'window_switch', 'buffer_zoomed', 'buffer_unzoomed']
)

bar_item_cmd_btn('btn_scroll_unread', '[u]', '/window scroll_unread')
bar_item_cmd_btn('btn_set_unread', '[U]', '/input set_unread_current_buffer')

bar_item_cmd_btn('btn_clear', '/CL', '/CL')
bar_item_cmd_btn('btn_clearhot', '/CLH', '/CLH')
bar_item_cmd_btn('btn_close', '/C', '/C')
bar_item_cmd_btn('btn_quit', '/BYE', '/BYE')

bar_item_cmd_btn('btn_bare', 'BARE', '/window bare')
bar_item_cmd_btn('btn_connect_relay', 'CON~', '/connect_relay')
bar_item_cmd_btn('btn_disconnect', 'CON×', '/disconnect -all')

bar_item_cmd_btn('btn_win_close', '[×]', '/window close -window ${if:"${_window_number}"=="*"?${window.number}:${_window_number}}')
bar_item_cmd_btn(
    'btn_win_zoom',
    '${if:${zoom}?${color:white,red}[¬]${color:reset}:[¬]}',
    '/window zoom -window ${if:"${_window_number}"=="*"?${window.number}:${_window_number}}',
    ['window_switch', 'window_zoomed', 'window_unzoomed']
)
bar_item_cmd_btn('btn_win_only', '[*]', '/window merge -window ${if:"${_window_number}"=="*"?${window.number}:${_window_number}} all')
bar_item_cmd_btn('btn_win_split', '[s]', '/window splith -window ${if:"${_window_number}"=="*"?${window.number}:${_window_number}}')
bar_item_cmd_btn('btn_win_vsplit', '[v]', '/window splitv -window ${if:"${_window_number}"=="*"?${window.number}:${_window_number}}')
bar_item_cmd_btn('btn_win_grow', '[+]', '/window resize -window ${if:"${_window_number}"=="*"?${window.number}:${_window_number}} +5')
bar_item_cmd_btn('btn_win_shrink', '[-]', '/window resize -window ${if:"${_window_number}"=="*"?${window.number}:${_window_number}} -5')

bar_item_cmd_btn('btn_toggle_buflist', 'BUFL', '/TOGGLE_BUFLIST')
bar_item_cmd_btn('btn_toggle_nicklist', 'NICKL', '/TOGGLE_NICKLIST')
bar_item_cmd_btn('btn_toggle_prefix', 'PREF', '/TOGGLE_PREFIX')
bar_item_cmd_btn('btn_toggle_time', 'TIME', '/TOGGLE_TIME')

bar = w.bar_search('fn')
if bar:
    w.bar_remove(bar)
w.bar_new(
    'fn', 'on', '200', 'root', '',
    'right', 'columns_horizontal', 'vertical',
    '0', '0', 'default', 'default', 'default', 'on',
    ', ,'.join([
        'btn_connect_relay',
        'btn_disconnect',
        'btn_toggle_buflist',
        'btn_toggle_nicklist',
        'btn_toggle_time',
        'btn_toggle_prefix',
        'btn_bare',
        'btn_layout_save',
        'btn_layout_reset',
        'btn_tab_next',
        'btn_tab_prev',
        'btn_tab_del',
        'btn_quit',
        'btn_close',
        'btn_clear',
        'btn_clearhot',
    ])
)
bar_item_cmd_btn('btn_fn', '>>', '/bar toggle fn')

bar = w.bar_search('win_fn')
if bar:
    w.bar_remove(bar)
w.bar_new(
    'win_fn', 'on', '0', 'window', '',
    'top', 'columns_horizontal', 'vertical',
    '0', '0', 'default', 'default', '233', 'off',
    ','.join([
        'btn_win_close',
        'btn_win_zoom',
        'btn_win_only',
        'btn_win_split',
        'btn_win_vsplit',
        'btn_win_grow',
        'btn_win_shrink',
    ])
)
bar_item_cmd_btn('btn_win_fn', '>>', '/bar toggle win_fn')


def cb_timer_startup(data, remaining_calls):
    cmd('tab_go 1')
    cmd('layout_reset core')
    cmd('tab_go 2')
    cmd('layout_reset bitlbee')
    cmd('tab_go 3')
    cmd('layout_reset twitter')
    cmd('tab_go 4')
    cmd('layout_reset #archlinux')
    cmd('tab_go 1')
    return w.WEECHAT_RC_OK


w.hook_timer(1, 0, 1, 'cb_timer_startup', '')

key_bind('mouse', '@item(buffer_name):button1', '/input switch_active_buffer')
key_bind('mouse', '@item(buffer_short_name):button1', '/input switch_active_buffer')
key_bind('mouse', '@item(mode_indicator):button1', '/vimode_go_to_normal')
key_bind('mouse', '@item(scroll):button1', '/window scroll_bottom')


def adjust_for_width():
    term_width = int(w.info_get('term_width', ''))
    cmd('/bar hide win_fn')
    if term_width < 80:
        cmd('/bar show fn')
        cmd('/bar hide buflist')
        cmd('/set weechat.look.prefix_align none')
    else:
        cmd('/bar hide fn')
        cmd('/bar show buflist')
        cmd('/set weechat.look.prefix_align right')


def cb_signal_sigwinch(data, signal, buffer):
    adjust_for_width()
    return w.WEECHAT_RC_OK


w.hook_signal('signal_sigwinch', 'cb_signal_sigwinch', '')
adjust_for_width()


def cb_signal_post_switch(data, signal, buffer):
    term_width = int(w.info_get('term_width', ''))
    if term_width < 80:
        cmd('/bar hide buflist')
    return w.WEECHAT_RC_OK


w.hook_signal('buffer_switch', 'cb_signal_post_switch', '')
w.hook_signal('window_switch', 'cb_signal_post_switch', '')

# }}}
