# -*- coding: utf-8 -*-
# vim: fdm=marker

from subprocess import Popen, PIPE
from types import SimpleNamespace
import functools
import re

import weechat as w

SCRIPT_NAME = 'custom'
SCRIPT_AUTHOR = 'Miroslav Koškár <http://mkoskar.com/>'
SCRIPT_VERSION = '0.1'
SCRIPT_LICENSE = 'Apache 2.0'
SCRIPT_DESC = 'Personal customizations'

w.register(
    SCRIPT_NAME,
    SCRIPT_AUTHOR,
    SCRIPT_VERSION,
    SCRIPT_LICENSE,
    SCRIPT_DESC,
    '',
    ''
)


# General {{{
# ----------------------------------------

config_local = w.config_new('local', '', '')

hd_bar = w.hdata_get('bar')
hd_bar_item = w.hdata_get('bar_item')
hd_bar_win = w.hdata_get('bar_window')
hd_buf = w.hdata_get('buffer')
hd_channel = w.hdata_get('irc_channel')
hd_config_file = w.hdata_get('config_file')
hd_config_option = w.hdata_get('config_option')
hd_config_section = w.hdata_get('config_section')
hd_layout = w.hdata_get('layout')
hd_line = w.hdata_get('line')
hd_line_data = w.hdata_get('line_data')
hd_lines = w.hdata_get('lines')
hd_server = w.hdata_get('irc_server')
hd_win = w.hdata_get('window')
hd_win_scroll = w.hdata_get('window_scroll')


def buffers_iter(start=None, forward=True, wrap=True):
    ref_start = 'gui_buffers' if forward else 'last_gui_buffer'
    ref_next = 'next_buffer' if forward else 'prev_buffer'
    item = start if start else w.hdata_get_list(hd_buf, ref_start)
    while item:
        yield item
        item = w.hdata_pointer(hd_buf, item, ref_next)
        if start and wrap and not item:
            item = w.hdata_get_list(hd_buf, ref_start)
        if start == item:
            break


def channels_iter(server):
    item = w.hdata_pointer(hd_server, server, 'channels')
    while item:
        yield item
        item = w.hdata_pointer(hd_channel, item, 'next_channel')


def layouts_iter():
    item = w.hdata_get_list(hd_layout, 'gui_layouts')
    while item:
        yield item
        item = w.hdata_pointer(hd_layout, item, 'next_layout')


def layouts_name_iter():
    for layout in layouts_iter():
        yield w.hdata_string(hd_layout, layout, 'name')


def options_iter(section):
    item = w.hdata_pointer(hd_config_section, section, 'options')
    while item:
        yield item
        item = w.hdata_pointer(hd_config_option, item, 'next_option')


def servers_iter():
    item = w.hdata_get_list(hd_server, 'irc_servers')
    while item:
        yield item
        item = w.hdata_pointer(hd_server, item, 'next_server')


def windows_buffer_iter():
    for window in windows_iter():
        yield window, w.hdata_pointer(hd_win, window, 'buffer')


def windows_iter():
    item = w.hdata_get_list(hd_win, 'gui_windows')
    while item:
        yield item
        item = w.hdata_pointer(hd_win, item, 'next_window')


# ----------------------------------------

core = w.buffer_search_main()


def cmd(command, buffer=core, mute=True):
    w.command(buffer, ('/mute ' if mute else '/') + command)


def key_bind(ctxt, key, val):
    cmd(f'key bindctxt {ctxt} {key} {val}')


def msg(message, buffer):
    w.command(buffer, message)


def server_opt(server_name, opt_name):
    opt = w.config_get(f'irc.server.{server_name}.{opt_name}')
    if w.config_option_is_null(opt):
        opt = w.config_get(f'irc.server_default.{opt_name}')
    return opt


def tag_by_prefix(tags, prefix):
    tag = next(filter(lambda i: i.startswith(prefix), tags), None)
    return tag.lstrip(prefix) if tag else None


# ----------------------------------------

def cb_build_bar_item_cmd_btn(data, item, window, buffer, extra_info):
    item_name = w.hdata_string(hd_bar_item, item, 'name')
    extra_vars = {}
    if item_name == 'btn_win_zoom':
        extra_vars['zoom'] = '1' if layout_find('_zoom') else '0'
    return w.string_eval_expression(data, {
        'window': window, 'buffer': buffer}, extra_vars, {})


def cb_signal_bar_item_cmd_btn_update(data, signal, signal_data):
    w.bar_item_update(data)
    return w.WEECHAT_RC_OK


def bar_item_cmd_btn(name, label, cmd, update_on=[]):
    w.bar_item_new('(extra)' + name, 'cb_build_bar_item_cmd_btn', label)
    w.bar_item_update(name)
    key_bind('mouse', f'@item({name}):button1', cmd)
    for signal in update_on:
        w.hook_signal(signal, 'cb_signal_bar_item_cmd_btn_update', name)

# }}}


# Buffers {{{
# ----------------------------------------

BUFFER_MERGE_RULES = [
    re.compile(r'^irc\.[^.]*\.#(bash|zsh)$'),
    re.compile(r'^irc\.[^.]*\.#archlinux(-offtopic)?$'),
    re.compile(r'^irc\.[^.]*\.#libera(-offtopic)?$'),
]

re_archlinux = re.compile(r'^#archlinux(|-.*)$')
re_bitlbee_control = re.compile(r'^irc\.bitlbee[^.]*\.&')
re_offtopic = re.compile(r'^(.*)-offtopic$')
re_twitter = re.compile(r'^irc\.bitlbee[^.]*\.#twitter_mkoskar$')

timer_buffer_sort_merge = None


def buffer_merge(buffer):
    merge_group = None
    bname = w.buffer_get_string(buffer, 'full_name')
    for idx, rule in enumerate(BUFFER_MERGE_RULES):
        if rule.match(bname):
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


def buffer_sort_merged():
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


def cb_timer_buffer_sort_merge(data, remaining_calls):
    global timer_buffer_sort_merge
    buffer_sort_merged()
    timer_buffer_sort_merge = None
    return w.WEECHAT_RC_OK


def buffer_init(buffer):
    bname = w.buffer_get_string(buffer, 'full_name')
    bname_ = bname.split('.')
    btype = w.buffer_get_string(buffer, 'localvar_type')

    if btype == 'server':
        w.buffer_set(buffer, 'short_name', '@' + bname_[2])
    elif btype == 'private':
        w.buffer_set(buffer, 'short_name', '+' + bname_[2])
    elif btype == 'channel':
        name = bname_[2]
        w.buffer_set(buffer, 'nicklist', '0')

        if re_twitter.match(bname):
            name = '#twitter'
            w.buffer_set(buffer, 'highlight_words', '@mkoskar')

        elif re_bitlbee_control.match(bname):
            w.buffer_set(buffer, 'nicklist', '1')

        if m := re_archlinux.match(name):
            name = '#arch' + m.group(1)

        if m := re_offtopic.match(name):
            name = m.group(1) + '-ot'

        w.buffer_set(buffer, 'short_name', name)

    w.buffer_set(buffer, 'time_for_each_line', '0')
    if bname == 'perl.highmon':
        w.buffer_set(buffer, 'time_for_each_line', '1')
    buffer_merge(buffer)
    global timer_buffer_sort_merge
    if not timer_buffer_sort_merge:
        timer_buffer_sort_merge = w.hook_timer(5, 0, 1, 'cb_timer_buffer_sort_merge', '')


def cb_signal_buffer_opened(data, signal, buffer):
    buffer_init(buffer)
    return w.WEECHAT_RC_OK


w.hook_signal('buffer_opened', 'cb_signal_buffer_opened', '')
w.hook_signal('irc_channel_opened', 'cb_signal_buffer_opened', '')
w.hook_signal('irc_pv_opened', 'cb_signal_buffer_opened', '')
w.hook_signal('irc_server_opened', 'cb_signal_buffer_opened', '')

for buffer in buffers_iter():
    buffer_init(buffer)


def buffer_hot_next(buffer, forward=True):
    iter = buffers_iter(buffer, forward)
    next(iter)
    for buf in iter:
        if w.hdata_pointer(hd_buf, buf, 'hotlist'):
            w.buffer_set(buf, 'display', '1')
            break


def cb_command_buffer_hot_next(data, buffer, args):
    buffer_hot_next(buffer, True)
    return w.WEECHAT_RC_OK


def cb_command_buffer_hot_prev(data, buffer, args):
    buffer_hot_next(buffer, False)
    return w.WEECHAT_RC_OK


w.hook_command('buffer_hot_next', '', '', '', '', 'cb_command_buffer_hot_next', '')
w.hook_command('buffer_hot_prev', '', '', '', '', 'cb_command_buffer_hot_prev', '')

# }}}


# Layouts {{{
# ----------------------------------------

def layout_find(name):
    return next((n for n in layouts_name_iter() if n == name), None)


def layout_current():
    return w.hdata_get_list(hd_layout, 'gui_layout_current')


def layout_current_name():
    cur = layout_current()
    return w.hdata_string(hd_layout, cur, 'name') if cur else None


def layout_move(src, dst):
    cmd(f'layout rename {src} {dst}')
    cmd(f'layout rename {src}_saved {dst}_saved')
    preset = w.buffer_get_string(core, f'localvar_{src}_preset')
    w.buffer_set(core, f'localvar_set_{dst}_preset', preset)
    w.buffer_set(core, f'localvar_del_{src}_preset', '')


def layout_del(src):
    cmd(f'layout del {src}')
    cmd(f'layout del {src}_saved')
    w.buffer_set(core, f'localvar_del_{src}_preset', '')


def cb_command_layout_reset(data, buffer, args):
    cmd('layout del _zoom')
    cur = layout_current_name()
    if not args:
        if cur is not None:
            preset = w.buffer_get_string(
                core, f'localvar_{cur}_preset'
            )
            if preset:
                cmd(f'layout_reset {preset}')
            else:
                cmd(f'layout apply {cur}_saved windows')
                cmd(f'layout store {cur} windows')
        return w.WEECHAT_RC_OK
    if args == '-':
        if cur is not None:
            cmd(f'layout del {cur}_saved')
            w.buffer_set(core, f'localvar_del_{cur}_preset', '')
        return w.WEECHAT_RC_OK
    cmd('window merge all')
    cmd('window splith 20')
    cmd('buffer perl.highmon')
    cmd('window down')
    cmd(f'buffer {args}')
    if cur is not None:
        cmd(f'layout store {cur} windows')
        w.buffer_set(core, f'localvar_set_{cur}_preset', args)
    return w.WEECHAT_RC_OK


def cb_command_layout_save(data, buffer, args):
    cur = layout_current_name()
    if cur is not None:
        cmd(f'layout store {cur}_saved windows')
        cmd(f'layout store {cur} windows')
        w.buffer_set(core, f'localvar_del_{cur}_preset', '')
    return w.WEECHAT_RC_OK


def cb_signal_delete_all_layouts_on_quit(data, signal, args):
    for layout_name in list(layouts_name_iter()):
        cmd(f'layout del {layout_name}')
    return w.WEECHAT_RC_OK


w.hook_command('layout_reset', '', '', '', '', 'cb_command_layout_reset', '')
w.hook_command('layout_save', '', '', '', '', 'cb_command_layout_save', '')
w.hook_signal('quit', 'cb_signal_delete_all_layouts_on_quit', '')

bar_item_cmd_btn('btn_layout_reset', 'RESET', '/layout_reset')
bar_item_cmd_btn('btn_layout_save', 'SAVE', '/layout_save')

key_bind('default', 'meta-space', '/layout_reset')
key_bind('default', 'meta-;meta-space', '/layout_save')
key_bind('default', 'meta-;meta-1', '/layout_reset core.weechat')
key_bind('default', 'meta-;meta-2', '/layout_reset &bitlbee')
key_bind('default', 'meta-;meta-3', '/layout_reset #twitter')

# }}}


# Tabs {{{
# ----------------------------------------

re_tab = re.compile(r'^tab(\d)$')


def str2tab(str):
    try:
        tab = int(str)
    except (TypeError, ValueError):
        return None
    if not 0 < tab < 10:
        return None
    return tab


def layout_tab(layout_name):
    if layout_name:
        if m := re_tab.match(layout_name):
            return str2tab(m.group(1))
    return None


def tabs_all():
    tabs = set()
    for layout_name in layouts_name_iter():
        tab = layout_tab(layout_name)
        if tab is not None:
            tabs.add(tab)
    return sorted(tabs)


def cb_command_tab_del(data, buffer, args):
    tab_cur = layout_tab(layout_current_name())
    if tab_cur is None:
        return w.WEECHAT_RC_ERROR
    tabs = tabs_all()
    if len(tabs) > 1:
        if tab_cur == tabs[-1]:
            cmd('tab_go prev')
        else:
            cmd('tab_go next')
    layout_del(f'tab{tab_cur}')
    bar_item_update_tabs()
    return w.WEECHAT_RC_OK


def cb_command_tab_go(data, buffer, args):
    tabs = tabs_all()
    tab_cur = layout_tab(layout_current_name())
    if args == 'prev':
        if tab_cur is None:
            tab_dst = tabs[0] if len(tabs) > 0 else None
        else:
            idx = tabs.index(tab_cur)
            tab_dst = tabs[(idx - 1) % len(tabs)]
    elif args == 'next':
        if tab_cur is None:
            tab_dst = tabs[-1] if len(tabs) > 0 else None
        else:
            idx = tabs.index(tab_cur)
            tab_dst = tabs[(idx + 1) % len(tabs)]
    elif args == 'last_visited':
        last_visited = w.buffer_get_string(core, 'localvar_tab_last_visited')
        cmd(f'tab_go {last_visited}')
        return w.WEECHAT_RC_OK
    else:
        tab_dst = str2tab(args)
    if tab_dst is None:
        return w.WEECHAT_RC_ERROR
    cmd('layout apply _zoom windows')
    cmd('layout del _zoom')
    if tab_cur is not None:
        cmd(f'layout store tab{tab_cur} windows')
        w.buffer_set(core, 'localvar_set_tab_last_visited', str(tab_cur))
    if layout_find(f'tab{tab_dst}'):
        cmd(f'layout apply tab{tab_dst} windows')
    else:
        cmd(f'layout store tab{tab_dst} windows')
        cmd('layout_reset')
    bar_item_update_tabs()
    return w.WEECHAT_RC_OK


def cb_command_tab_move(data, buffer, args):
    tab_dst = str2tab(args)
    if tab_dst is None:
        return w.WEECHAT_RC_ERROR
    tab_cur = layout_tab(layout_current_name())
    cmd('layout apply _zoom windows')
    cmd('layout del _zoom')
    if tab_cur is None:
        layout_del(tab_dst)
        cmd(f'layout store tab{tab_dst} windows')
    else:
        layout_move(tab_dst, 'tmp')
        cmd(f'layout store tab{tab_cur} windows')
        layout_move(f'tab{tab_cur}', f'tab{tab_dst}')
        layout_move('tmp', tab_cur)
        cmd(f'layout apply tab{tab_dst} windows')
    bar_item_update_tabs()
    return w.WEECHAT_RC_OK


w.hook_command('tab_del', '', '', '', '', 'cb_command_tab_del', '')
w.hook_command('tab_go', '', '', '', '', 'cb_command_tab_go', '')
w.hook_command('tab_move', '', '', '', '', 'cb_command_tab_move', '')


def cb_bar_item_tab(data, item, window):
    tab = str2tab(data)
    tab_cur = layout_tab(layout_current_name())
    if tab not in tabs_all():
        return ''
    return w.string_eval_expression(
        f'${{color:white,red}}{{{tab}}}${{color:reset}}'
        if tab == tab_cur else f'{{{tab}}}', {}, {}, {})


def cb_hsignal_tab_click(data, signal, hashtable):
    col = int(hashtable['_bar_item_col'])
    if col > 2:
        return w.WEECHAT_RC_OK
    tab = str2tab(data)
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
bar_item_cmd_btn('btn_tab_next', 'TAB+', '/tab_go next')
bar_item_cmd_btn('btn_tab_prev', 'TAB-', '/tab_go prev')

key_bind('default', 'meta-!', '/tab_move 1')
key_bind('default', 'meta-@', '/tab_move 2')
key_bind('default', 'meta-#', '/tab_move 3')
key_bind('default', 'meta-$', '/tab_move 4')
key_bind('default', 'meta-%', '/tab_move 5')
key_bind('default', 'meta-^', '/tab_move 6')
key_bind('default', 'meta-&', '/tab_move 7')
key_bind('default', 'meta-*', '/tab_move 8')
key_bind('default', 'meta-(', '/tab_move 9')

key_bind('default', 'meta-0', '/tab_del')
key_bind('default', 'meta-h', '/tab_go prev')
key_bind('default', 'meta-l', '/tab_go next')
key_bind('default', 'meta-a', '/tab_go last_visited')

# }}}


# Relay servers
# ----------------------------------------

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
            if not w.config_boolean(server_opt(name, 'autoconnect')):
                continue
            connect_relay(name)
    else:
        connect_relay(args)
    return w.WEECHAT_RC_OK


def cb_completion_irc_servers(data, completion_item, buffer, completion):
    for server in servers_iter():
        name = w.hdata_string(hd_server, server, 'name')
        if name.endswith('~'):
            continue
        w.hook_completion_list_add(completion, name, 0, w.WEECHAT_LIST_POS_END)
    return w.WEECHAT_RC_OK


w.hook_command('connect_relay', '', '', '', '%(irc_servers)', 'cb_command_connect_relay', '')
w.hook_completion('irc_servers', '', 'cb_completion_irc_servers', '')


# Execute a command on all buffers
# ----------------------------------------

def cb_command_allbuf(data, buffer, args):
    for buf in buffers_iter():
        cmd(args, buffer=buf)
    return w.WEECHAT_RC_OK


w.hook_command('allbuf', '', '', '', '', 'cb_command_allbuf', '')


# Add/Delete a filter for a nick
# ----------------------------------------

def cb_command_grep_nick(data, buffer, args):
    bname = w.buffer_get_string(buffer, 'full_name')
    cmd(f'filter del grep_{bname}')
    if args:
        cmd(f'filter add grep_{bname} {bname} !nick_{args} *')
    return w.WEECHAT_RC_OK


w.hook_command('grep_nick', '', '', '', '', 'cb_command_grep_nick', '')

cmd('alias add GNICK grep_nick')


# Regain "nick" from "nick_"
# ----------------------------------------

def cb_command_renick(data, buffer, args):
    nick_old = w.buffer_get_string(buffer, 'localvar_nick')
    if not nick_old:
        return w.WEECHAT_RC_ERROR
    nick_new = nick_old.rstrip('_')
    if nick_new == nick_old:
        return w.WEECHAT_RC_OK
    cmd(f'msg NickServ ghost {nick_new}', buffer)
    cmd(f'msg NickServ release {nick_new}', buffer)
    cmd(f'nick {nick_new}', buffer)
    return w.WEECHAT_RC_OK


w.hook_command('renick', '', '', '', '', 'cb_command_renick', '')


# Mark a buffer as read
# ----------------------------------------

def set_read(buffer):
    w.buffer_set(buffer, 'hotlist', '-1')
    w.buffer_set(buffer, 'unread', '-')
    if w.buffer_get_integer(buffer, 'num_displayed'):
        for win, buf in windows_buffer_iter():
            if buffer == buf:
                wnum = w.window_get_integer(win, 'number')
                cmd(f'window scroll_bottom -window {wnum}')


def cb_command_set_read(data, buffer, args):
    if not w.hdata_pointer(hd_buf, buffer, 'mixed_lines') \
       or w.buffer_get_integer(buffer, 'zoomed'):
        set_read(buffer)
    else:
        bnum = w.buffer_get_integer(buffer, 'number')
        for buf in buffers_iter():
            if bnum == w.buffer_get_integer(buf, 'number'):
                set_read(buf)
    return w.WEECHAT_RC_OK


def cb_command_set_read_all(data, buffer, args):
    for buf in buffers_iter():
        set_read(buf)
    return w.WEECHAT_RC_OK


w.hook_command('set_read', '', '', '', '', 'cb_command_set_read', '')
w.hook_command('set_read_all', '', '', '', '', 'cb_command_set_read_all', '')

cmd('alias add R set_read')
cmd('alias add RA set_read_all')

key_bind('default', 'meta-ctrl-M', '/set_read')


# Open/Yank urls
# ----------------------------------------

def cb_command_urls(data, buffer, args):
    win = w.current_window()
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
            p = Popen('urls -o', shell=True, stdin=PIPE)
        elif data == 'yank':
            p = Popen('urls -y', shell=True, stdin=PIPE)
        else:
            return w.WEECHAT_RC_ERROR
        count = 80
        while count > 0 and line:
            ldata = w.hdata_pointer(hd_line, line, 'data')
            if not w.hdata_char(hd_line_data, ldata, 'displayed'):
                line = w.hdata_pointer(hd_line, line, 'prev_line')
                continue
            lmsg = w.hdata_string(hd_line_data, ldata, 'message')
            p.stdin.write(f"{w.string_remove_color(lmsg, '')}\n".encode())
            line = w.hdata_pointer(hd_line, line, 'prev_line')
            count -= 1
        p.stdin.close()
        p.wait()
    except Exception:
        return w.WEECHAT_RC_ERROR
    return w.WEECHAT_RC_OK


w.hook_command('urls_open', '', '', '', '', 'cb_command_urls', 'open')
w.hook_command('urls_yank', '', '', '', '', 'cb_command_urls', 'yank')


# Strip commands for whitespace
# ----------------------------------------

def cb_command_run_strip_input(data, buffer, command):
    input_line = w.buffer_get_string(buffer, 'input')
    w.buffer_set(buffer, 'input', input_line.strip())
    return w.WEECHAT_RC_OK


w.hook_command_run('/input return', 'cb_command_run_strip_input', '')


# Autorespond
# ----------------------------------------

def autorespond_handle_privmsg(ctx):
    pass


def autorespond_cb_print_privmsg(data, buffer, date, tags,
                                 displayed, highlight, prefix, message):
    bname = w.buffer_get_string(buffer, 'full_name')
    bnick = w.buffer_get_string(buffer, 'localvar_nick')
    tags = tags.split(',')
    self_msg = 'self_msg' in tags
    host = tag_by_prefix(tags, 'host_')
    nick = tag_by_prefix(tags, 'nick_')
    ctx = SimpleNamespace(**locals())
    autorespond_handle_privmsg(ctx)
    return w.WEECHAT_RC_OK


w.hook_print('', 'irc_privmsg', '', 1, 'autorespond_cb_print_privmsg', '')


# Local per-server configuration
# ----------------------------------------

def local_server_opt(server_name, opt_name):
    opt = w.config_get(f'local.server.{server_name}.{opt_name}')
    if not opt:
        if opt_name == 'autojoin':
            opt = w.config_new_option(
                config_local, config_local_server, f'{server_name}.{opt_name}',
                'string', '', '', 0, 0, '', '', 0, '', '', '', '', '', ''
            )
    return opt


def config_local_server_read_cb(data, config_file, section, option_name, value):
    try:
        server_name, server_opt_name = option_name.split('.')
    except ValueError:
        return w.WEECHAT_CONFIG_OPTION_SET_OPTION_NOT_FOUND
    if not server_name or not server_opt_name:
        return w.WEECHAT_CONFIG_OPTION_SET_OPTION_NOT_FOUND
    opt = local_server_opt(server_name, server_opt_name)
    w.config_option_set(opt, value, 1)
    return w.WEECHAT_CONFIG_OPTION_SET_OK_CHANGED


def cb_signal_save_autojoins_on_quit(data, signal, args):
    for server in servers_iter():
        sname = w.hdata_string(hd_server, server, 'name')
        if w.hdata_integer(hd_server, server, 'temp_server') > 0:
            continue
        autojoin = []
        for channel in channels_iter(server):
            chname = w.hdata_string(hd_channel, channel, 'name')
            chtype = w.hdata_integer(hd_channel, channel, 'type')
            if chtype == 0:
                autojoin.append(chname)
        opt = local_server_opt(sname, 'autojoin')
        if autojoin:
            autojoin.sort()
            w.config_option_set(opt, ','.join(autojoin), 1)
    w.config_write(config_local)
    return w.WEECHAT_RC_OK


w.hook_signal('quit', 'cb_signal_save_autojoins_on_quit', '')

config_local_server = w.config_new_section(
    config_local, 'server', 1, 1,
    'config_local_server_read_cb', '', '', '', '', '', '', '', '', ''
)
for server in servers_iter():
    sname = w.hdata_string(hd_server, server, 'name')
    if w.hdata_integer(hd_server, server, 'temp_server') > 0:
        continue
    local_server_opt(sname, 'autojoin')


# ----------------------------------------

bar_item_cmd_btn(
    'btn_filter',
    '[${if:${buffer.filter}?F:f}${if:${info:filters_enabled}?+:-}]',
    '/filter toggle @',
    ['buffer_switch', 'window_switch',
     'buffer_filters_enabled', 'buffer_filters_disabled',
     'filters_enabled', 'filters_disabled']
)

bar_item_cmd_btn(
    'btn_nicklist_count',
    '${if:${type}==channel?${color:default}${buffer.nicklist_nicks_count}${color:reset}}',
    '/TOGGLE_NICKLIST',
    ['buffer_switch']
)

bar_item_cmd_btn(
    'btn_server',
    '${if:${type}==private||${type}==channel?@ ${server}}',
    '/server jump',
    ['buffer_switch', 'window_switch']
)

bar_item_cmd_btn(
    'btn_zoom',
    '${if:${buffer.mixed_lines}!=0x0?[${if:${buffer.zoomed}?Z:z}]}',
    '/input zoom_merged_buffer',
    ['buffer_switch', 'window_switch', 'buffer_zoomed', 'buffer_unzoomed']
)

bar_item_cmd_btn('btn_scroll_unread', '[U]', '/window scroll_unread')
bar_item_cmd_btn('btn_set_read', '[R]', '/R')
bar_item_cmd_btn('btn_set_read_all', '/RA', '/RA')

bar_item_cmd_btn('btn_clear', '/CL', '/CL')
bar_item_cmd_btn('btn_close', '/C', '/C')
bar_item_cmd_btn('btn_quit', '/BYE', '/BYE')

bar_item_cmd_btn('btn_bare', 'BARE', '/window bare')
bar_item_cmd_btn('btn_connect_relay', 'CON~', '/connect_relay')
bar_item_cmd_btn('btn_disconnect', 'CON×', '/disconnect -all')

bar_item_cmd_btn('btn_win_close', '[×]', '/window close -window ${if:${_window_number}==*?${window.number}:${_window_number}}')
bar_item_cmd_btn(
    'btn_win_zoom',
    '${if:${zoom}?${color:white,red}[¬]${color:reset}:[¬]}',
    '/window zoom -window ${if:${_window_number}==*?${window.number}:${_window_number}}',
    ['window_switch', 'window_zoomed', 'window_unzoomed']
)
bar_item_cmd_btn('btn_win_only', '[*]', '/window merge -window ${if:${_window_number}==*?${window.number}:${_window_number}} all')
bar_item_cmd_btn('btn_win_split', '[s]', '/window splith -window ${if:${_window_number}==*?${window.number}:${_window_number}}')
bar_item_cmd_btn('btn_win_vsplit', '[v]', '/window splitv -window ${if:${_window_number}==*?${window.number}:${_window_number}}')
bar_item_cmd_btn('btn_win_grow', '[+]', '/window resize -window ${if:${_window_number}==*?${window.number}:${_window_number}} +5')
bar_item_cmd_btn('btn_win_shrink', '[-]', '/window resize -window ${if:${_window_number}==*?${window.number}:${_window_number}} -5')

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
    '0', '0', 'default', 'default', 'default', 'default', 'on',
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
        'btn_set_read_all',
    ])
)
bar_item_cmd_btn('btn_fn', '>>', '/bar toggle fn')

bar = w.bar_search('win_fn')
if bar:
    w.bar_remove(bar)
w.bar_new(
    'win_fn', 'on', '0', 'window', '',
    'top', 'columns_horizontal', 'vertical',
    '0', '0', 'default', 'default', '233', '233', 'off',
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

key_bind('mouse', '@item(buffer_name):button1', '/input switch_active_buffer')
key_bind('mouse', '@item(buffer_short_name):button1', '/input switch_active_buffer')
key_bind('mouse', '@item(mode_indicator):button1', '/vimode_go_to_normal')
key_bind('mouse', '@item(scroll):button1', '/window scroll_bottom')


# ----------------------------------------

def adjust_for_width():
    term_width = int(w.info_get('term_width', ''))
    cmd('bar hide win_fn')
    if term_width < 80:
        cmd('bar show fn')
        cmd('bar hide buflist')
        cmd('set weechat.look.prefix_align none')
        cmd('set weechat.look.align_end_of_lines prefix')
        cmd('set weechat.look.prefix_align_max 6')
        cmd('set weechat.look.prefix_align_min 4')
    else:
        cmd('bar hide fn')
        cmd('bar show buflist')
        cmd('set weechat.look.prefix_align right')
        cmd('set weechat.look.align_end_of_lines message')
        cmd('set weechat.look.prefix_align_max 10')
        cmd('set weechat.look.prefix_align_min 8')


def cb_signal_sigwinch(data, signal, signal_data):
    adjust_for_width()
    return w.WEECHAT_RC_OK


def cb_signal_hide_buflist_by_width(data, signal, signal_data):
    term_width = int(w.info_get('term_width', ''))
    if term_width < 80:
        cmd('bar hide buflist')
    return w.WEECHAT_RC_OK


w.hook_signal('signal_sigwinch', 'cb_signal_sigwinch', '')
w.hook_signal('buffer_switch', 'cb_signal_hide_buflist_by_width', '')
w.hook_signal('window_switch', 'cb_signal_hide_buflist_by_width', '')

adjust_for_width()


# ----------------------------------------

def cb_timer_startup(data, remaining_calls):
    cmd('tab_go 1')
    cmd('layout_reset core.weechat')
    cmd('tab_go 2')
    cmd('layout_reset &bitlbee')
    cmd('tab_go 3')
    cmd('layout_reset #twitter')
    cmd('tab_go 4')
    cmd('layout_reset #archlinux')
    cmd('tab_go 1')
    w.buffer_set(core, 'localvar_set_tab_last_visited', '')
    return w.WEECHAT_RC_OK


w.hook_timer(1, 0, 1, 'cb_timer_startup', '')

w.config_read(config_local)
