# -*- coding: utf-8 -*-
# vim: fdm=marker

from subprocess import Popen, PIPE
from types import SimpleNamespace
import functools
import itertools
import json
import re
import time

import weechat as w

w.register(
    'custom',
    'Miroslav Koškár <https://mkoskar.com/>',
    '0.1',
    'Apache 2.0',
    'Personal customizations',
    '',
    ''
)


# General {{{
# ----------------------------------------

core = w.buffer_search_main()
loaded = w.buffer_get_string(core, 'localvar_loaded_custom')

xdg = bool(w.info_get('weechat_config_dir', ''))
config_dir_ref = '${weechat_config_dir}' if xdg else '${weechat_dir}'

hd_bar_item = w.hdata_get('bar_item')
hd_buf = w.hdata_get('buffer')
hd_channel = w.hdata_get('irc_channel')
hd_config_file = w.hdata_get('config_file')
hd_config_option = w.hdata_get('config_option')
hd_config_section = w.hdata_get('config_section')
hd_layout = w.hdata_get('layout')
hd_line = w.hdata_get('line')
hd_line_data = w.hdata_get('line_data')
hd_lines = w.hdata_get('lines')
hd_nick = w.hdata_get('irc_nick')
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


def config_files_iter():
    item = w.hdata_get_list(hd_config_file, 'config_files')
    while item:
        yield item
        item = w.hdata_pointer(hd_config_file, item, 'next_config')


def config_sections_iter(config_file):
    item = w.hdata_pointer(hd_config_file, config_file, 'sections')
    while item:
        yield item
        item = w.hdata_pointer(hd_config_section, item, 'next_section')


def layouts_iter():
    item = w.hdata_get_list(hd_layout, 'gui_layouts')
    while item:
        yield item
        item = w.hdata_pointer(hd_layout, item, 'next_layout')


def layouts_name_iter():
    for layout in layouts_iter():
        yield w.hdata_string(hd_layout, layout, 'name')


def nicks_iter(channel):
    item = w.hdata_pointer(hd_channel, channel, 'nicks')
    while item:
        yield item
        item = w.hdata_pointer(hd_nick, item, 'next_nick')


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

def bar_addreplace(*args):
    bar = w.bar_search(args[0])
    if bar:
        w.bar_remove(bar)
    w.bar_new(*args)


def channel_search(server_name, channel_name):
    for server in servers_iter():
        if w.hdata_string(hd_server, server, 'name') != server_name:
            continue
        for channel in channels_iter(server):
            if w.hdata_string(hd_channel, channel, 'name') != channel_name:
                continue
            return channel
    return None


def cmd(command, buffer=core, mute=True):
    command = command.strip().replace('\n', ' ')
    w.command(buffer, ('/mute ' if mute else '/') + command)


def config_file_search(name):
    for config_file in config_files_iter():
        if w.hdata_string(hd_config_file, config_file, 'name') == name:
            return config_file
    return None


def key_bind(key, val, ctxt='default'):
    cmd(f'key bindctxt {ctxt} {key} {val}')


def msg(message, buffer):
    w.command(buffer, message)


def nick_search(channel, name):
    for nick in nicks_iter(channel):
        if w.hdata_string(hd_nick, nick, 'name') == name:
            return nick
    return None


def print_error(message, buffer=''):
    w.prnt(buffer, w.prefix('error') + message)


def server_opt(server_name, opt_name):
    opt = w.config_get(f'irc.server.{server_name}.{opt_name}')
    if w.config_option_is_null(opt):
        opt = w.config_get(f'irc.server_default.{opt_name}')
    return opt


def tag_by_prefix(tags, prefix):
    tag = next(filter(lambda i: i.startswith(prefix), tags), None)
    return tag.lstrip(prefix) if tag else None


# ----------------------------------------

def bar_item_cmd_btn(name, label, cmd, update_on=[]):
    w.bar_item_new('(extra)' + name, 'cb_build_bar_item_cmd_btn', label)
    w.bar_item_update(name)
    key_bind(f'@item({name}):button1', cmd, 'mouse')
    for signal in update_on:
        w.hook_signal(signal, 'cb_signal_bar_item_cmd_btn_update', name)


def cb_build_bar_item_cmd_btn(data, item, window, buffer, extra_info):
    item_name = w.hdata_string(hd_bar_item, item, 'name')
    extra_vars = {}
    if item_name == 'btn_win_zoom':
        extra_vars['zoom'] = '1' if layout_search('_zoom') else '0'
    return w.string_eval_expression(data, {'window': window, 'buffer': buffer}, extra_vars, {})


def cb_signal_bar_item_cmd_btn_update(data, signal, signal_data):
    w.bar_item_update(data)
    return w.WEECHAT_RC_OK

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

def layout_current():
    return w.hdata_get_list(hd_layout, 'gui_layout_current')


def layout_current_name():
    cur = layout_current()
    return w.hdata_string(hd_layout, cur, 'name') if cur else None


def layout_del(src):
    cmd(f'layout del {src}')
    cmd(f'layout del {src}_saved')
    w.buffer_set(core, f'localvar_del_{src}_preset', '')


def layout_move(src, dst):
    cmd(f'layout rename {src} {dst}')
    cmd(f'layout rename {src}_saved {dst}_saved')
    preset = w.buffer_get_string(core, f'localvar_{src}_preset')
    w.buffer_set(core, f'localvar_set_{dst}_preset', preset)
    w.buffer_set(core, f'localvar_del_{src}_preset', '')


def layout_search(name):
    for layout in layouts_iter():
        if w.hdata_string(hd_layout, layout, 'name') == name:
            return layout
    return None


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
    if args == '-del':
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
        cmd(f'layout del {cur}_saved')
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


def cb_signal_save_layout_presets(data, signal, args):
    presets = {}
    for layout_name in layouts_name_iter():
        preset = w.buffer_get_string(core, f'localvar_{layout_name}_preset')
        if preset:
            presets[layout_name] = preset
    cmd(f'set plugins.var.layout_presets "{json.dumps(presets)}"')
    return w.WEECHAT_RC_OK


w.hook_command('layout_reset', '', '', '',
               '%(irc_channels) || -del',
               'cb_command_layout_reset', '')
w.hook_command('layout_save', '', '', '', '', 'cb_command_layout_save', '')
w.hook_signal('quit', 'cb_signal_save_layout_presets', '')


bar_item_cmd_btn('btn_layout_reset', 'RESET', '/layout_reset')
bar_item_cmd_btn('btn_layout_save', 'SAVE', '/layout_save')

key_bind('meta-space', '/layout_reset')
key_bind('meta-;meta-space', '/layout_save')
key_bind('meta-;meta-1', '/layout_reset core.weechat')
key_bind('meta-;meta-2', '/layout_reset &bitlbee')
key_bind('meta-;meta-3', '/layout_reset #twitter')

if not loaded:
    opt = w.config_get('plugins.var.layout_presets')
    if opt:
        presets = json.loads(w.config_string(opt))
        for key, val in presets.items():
            w.buffer_set(core, f'localvar_set_{key}_preset', val)

# }}}


# Workspaces {{{
# ----------------------------------------

def str2wspace(str):
    try:
        ws = int(str)
    except (TypeError, ValueError):
        return None
    if not 0 < ws < 10:
        return None
    return ws


def layout2wspace(layout_name):
    if layout_name:
        if m := re.match(r'^ws(\d)$', layout_name):
            return str2wspace(m.group(1))
    return None


def wspaces_all():
    wspaces = set()
    for layout_name in layouts_name_iter():
        ws = layout2wspace(layout_name)
        if ws is not None:
            wspaces.add(ws)
    return sorted(wspaces)


def cb_command_wspace_del(data, buffer, args):
    cur = layout2wspace(layout_current_name())
    if cur is None:
        return w.WEECHAT_RC_ERROR
    wspaces = wspaces_all()
    if len(wspaces) > 1:
        if cur == wspaces[-1]:
            cmd('wspace_go prev')
        else:
            cmd('wspace_go next')
    layout_del(f'ws{cur}')
    bar_item_update_wspaces()
    return w.WEECHAT_RC_OK


def cb_command_wspace_go(data, buffer, args):
    wspaces = wspaces_all()
    cur = layout2wspace(layout_current_name())
    if args == 'prev':
        if cur is None:
            dst = wspaces[0] if len(wspaces) > 0 else None
        else:
            idx = wspaces.index(cur)
            dst = wspaces[(idx - 1) % len(wspaces)]
    elif args == 'next':
        if cur is None:
            dst = wspaces[-1] if len(wspaces) > 0 else None
        else:
            idx = wspaces.index(cur)
            dst = wspaces[(idx + 1) % len(wspaces)]
    elif args == 'last_visited':
        last_visited = w.buffer_get_string(core, 'localvar_wspace_last_visited')
        cmd(f'wspace_go {last_visited}')
        return w.WEECHAT_RC_OK
    else:
        dst = str2wspace(args)
    if dst is None:
        return w.WEECHAT_RC_ERROR
    cmd('layout apply _zoom windows')
    cmd('layout del _zoom')
    if cur is not None:
        cmd(f'layout store ws{cur} windows')
        w.buffer_set(core, 'localvar_set_wspace_last_visited', str(cur))
    if layout_search(f'ws{dst}'):
        cmd(f'layout apply ws{dst} windows')
    else:
        cmd(f'layout store ws{dst} windows')
        cmd('layout_reset')
    bar_item_update_wspaces()
    return w.WEECHAT_RC_OK


def cb_command_wspace_move(data, buffer, args):
    dst = str2wspace(args)
    if dst is None:
        return w.WEECHAT_RC_ERROR
    cur = layout2wspace(layout_current_name())
    cmd('layout apply _zoom windows')
    cmd('layout del _zoom')
    if cur is None:
        layout_del(dst)
        cmd(f'layout store ws{dst} windows')
    else:
        layout_move(dst, 'tmp')
        cmd(f'layout store ws{cur} windows')
        layout_move(f'ws{cur}', f'ws{dst}')
        layout_move('tmp', cur)
        cmd(f'layout apply ws{dst} windows')
    bar_item_update_wspaces()
    return w.WEECHAT_RC_OK


def cb_command_wspaces_reset(data, buffer, args):
    if args == '-defaults':
        cmd('wspace_go 1')
        cmd('layout_reset core.weechat')
        cmd('wspace_go 2')
        cmd('layout_reset &bitlbee')
        cmd('wspace_go 3')
        cmd('layout_reset #twitter')
        cmd('wspace_go 4')
        cmd('layout_reset #archlinux')
        for ws in range(5, 10):
            cmd(f'wspace_go {ws}')
            cmd('wspace_del')
        cmd('wspace_go 1')
        w.buffer_set(core, 'localvar_del_wspace_last_visited', '')
    else:
        cur = layout2wspace(layout_current_name())
        last_visited = w.buffer_get_string(core, 'localvar_wspace_last_visited')
        for ws in wspaces_all():
            cmd(f'wspace_go {ws}')
            cmd('layout_reset')
        cmd(f'wspace_go {cur}')
        w.buffer_set(core, 'localvar_set_wspace_last_visited', last_visited)
    return w.WEECHAT_RC_OK


w.hook_command('wspace_del', '', '', '', '', 'cb_command_wspace_del', '')
w.hook_command('wspace_go', '', '', '', '', 'cb_command_wspace_go', '')
w.hook_command('wspace_move', '', '', '', '', 'cb_command_wspace_move', '')
w.hook_command('wspaces_reset', '', '', '', '-defaults', 'cb_command_wspaces_reset', '')

cmd('alias add WRA wspaces_reset')


def cb_bar_item_wspace(data, item, window):
    ws = str2wspace(data)
    cur = layout2wspace(layout_current_name())
    if ws not in wspaces_all():
        return ''
    return w.string_eval_expression(
        f'${{color:white,red}}{{{ws}}}${{color:reset}}'
        if ws == cur else f'{{{ws}}}', {}, {}, {})


def cb_hsignal_wspace_click(data, signal, hashtable):
    col = int(hashtable['_bar_item_col'])
    if col > 2:
        return w.WEECHAT_RC_OK
    ws = str2wspace(data)
    if ws not in wspaces_all():
        return w.WEECHAT_RC_OK
    cmd(f'wspace_go {ws}')
    return w.WEECHAT_RC_OK


def bar_item_update_wspaces():
    for i in range(1, 10):
        w.bar_item_update(f'ws{i}')


bar_item_update_wspaces()

for i in range(1, 10):
    w.bar_item_new(f'ws{i}', 'cb_bar_item_wspace', str(i))
    w.hook_hsignal(f'ws{i}_click', 'cb_hsignal_wspace_click', str(i))
    key_bind(f'@item(ws{i}):button1', f'hsignal:ws{i}_click', 'mouse')
    key_bind(f'meta-{i}', f'/wspace_go {i}')
    cmd(f'alias add {i} wspace_go {i}')

bar_item_cmd_btn('btn_wspace_del', 'WS×', '/wspace_del')
bar_item_cmd_btn('btn_wspace_next', 'WS+', '/wspace_go next')
bar_item_cmd_btn('btn_wspace_prev', 'WS-', '/wspace_go prev')

key_bind('meta-!', '/wspace_move 1')
key_bind('meta-@', '/wspace_move 2')
key_bind('meta-#', '/wspace_move 3')
key_bind('meta-$', '/wspace_move 4')
key_bind('meta-%', '/wspace_move 5')
key_bind('meta-^', '/wspace_move 6')
key_bind('meta-&', '/wspace_move 7')
key_bind('meta-*', '/wspace_move 8')
key_bind('meta-(', '/wspace_move 9')

key_bind('meta-0', '/wspace_del')
key_bind('meta-h', '/wspace_go prev')
key_bind('meta-l', '/wspace_go next')
key_bind('meta-a', '/wspace_go last_visited')

# }}}


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


# Autorespond
# ----------------------------------------

def autorespond_handle(ctx):
    pass


def autorespond_cb_print(data, buffer, date, tags,
                         displayed, highlight, prefix, message):
    bname = w.buffer_get_string(buffer, 'localvar_name')
    btype = w.buffer_get_string(buffer, 'localvar_type')
    bnick = w.buffer_get_string(buffer, 'localvar_nick')
    bhost = w.buffer_get_string(buffer, 'localvar_host')
    server = w.buffer_get_string(buffer, 'localvar_server')
    channel = w.buffer_get_string(buffer, 'localvar_channel')
    tags = set(tags.split(','))
    self_msg = 'self_msg' in tags
    nick = tag_by_prefix(tags, 'nick_')
    host = tag_by_prefix(tags, 'host_')
    ctx = SimpleNamespace(**locals())
    autorespond_handle(ctx)
    return w.WEECHAT_RC_OK


w.hook_print('', 'irc_privmsg', '', 1, 'autorespond_cb_print', '')


# Execute a command on all buffers
# ----------------------------------------

def cb_command_allbuf(data, buffer, args):
    for buf in buffers_iter():
        cmd(args, buffer=buf)
    return w.WEECHAT_RC_OK


w.hook_command('allbuf', '', '', '', '', 'cb_command_allbuf', '')


# Ignore filter
# ----------------------------------------

ignore_filter_userhost_hook = None
re_ignore_filter = None


def ignore_filter_reload():
    global re_ignore_filter
    re_ignore_filter = {}
    for opt in options_iter(w.config_search_section(config_file_search('plugins'), 'var')):
        opt_name = w.hdata_string(hd_config_option, opt, 'name')
        key, _, subkey = opt_name.partition('.')
        if key == 'ignore_filter':
            if not subkey:
                continue
            re_ignore_filter[subkey] = list(map(
                lambda x: re.compile(x), w.config_string(opt).split(',')
            ))


def ignore_filter_add(key, item):
    opt = w.config_get(f'plugins.var.ignore_filter.{key}')
    items = set()
    if opt:
        items = set(w.config_string(opt).split(','))
    items.add(item)
    val = ','.join(sorted(items))
    cmd(f'set plugins.var.ignore_filter.{key} "{val}"')
    ignore_filter_reload()


def cb_hsignal_irc_redirection_ignore_filter_userhost(data, signal, hashtable):
    w.unhook(ignore_filter_userhost_hook)
    out = hashtable.get('output')
    if not out:
        return w.WEECHAT_RC_ERROR
    text = w.info_get_hashtable('irc_message_parse', {'message': out})['text']
    text = text.strip()
    if not text:
        return w.WEECHAT_RC_ERROR
    if m := re.match(r'^(.*)=[+-](.*)@(.*)$', text):
        ignore_filter_add(data, '^host_.*@{}$'.format(m.group(3)))
        return w.WEECHAT_RC_OK
    return w.WEECHAT_RC_ERROR


def cb_command_ignore_filter(data, buffer, args):
    args = args.split()
    if not args:
        cmd('set plugins.var.ignore_filter.*', mute=False)
        return w.WEECHAT_RC_OK
    if args[0] == 'toggle':
        cmd('filter toggle ignore')
        return w.WEECHAT_RC_OK
    if args[0] == 'del':
        if len(args) < 3:
            return w.WEECHAT_RC_ERROR
        if args[1] == '*':
            if args[2] == '-all':
                cmd('unset -mask plugins.var.ignore_filter.*')
            elif args[2] in {'-server', '-channel'}:
                server = w.buffer_get_string(buffer, 'localvar_server')
                if not server:
                    return w.WEECHAT_RC_ERROR
                server = server.rstrip('~')
                if args[2] == '-server':
                    cmd(f'unset -mask plugins.var.ignore_filter.{server}.*')
                else:
                    channel = w.buffer_get_string(buffer, 'localvar_channel')
                    if not channel:
                        return w.WEECHAT_RC_ERROR
                    cmd(f'unset plugins.var.ignore_filter.{server}.{channel}')
            else:
                return w.WEECHAT_RC_ERROR
        ignore_filter_reload()
        cmd('ignore_filter', mute=False)
        return w.WEECHAT_RC_OK
    server = w.buffer_get_string(buffer, 'localvar_server')
    if not server:
        return w.WEECHAT_RC_ERROR
    if args[0] in {'host', 'nick'}:
        if len(args) < 2:
            return w.WEECHAT_RC_ERROR
        if not w.info_get('irc_is_nick', server + ',' + args[1]):
            return w.WEECHAT_RC_ERROR
        key = server.rstrip('~')
        if len(args) > 2 and args[2] == '-channel':
            channel = w.buffer_get_string(buffer, 'localvar_channel')
            if not channel:
                return w.WEECHAT_RC_ERROR
            key = key + '.' + channel
        if args[0] == 'host':
            w.hook_hsignal_send('irc_redirect_command', {
                'server': server,
                'pattern': 'userhost',
                'signal': 'ignore_filter',
                'string': args[1],
            })
            global ignore_filter_userhost_hook
            ignore_filter_userhost_hook = w.hook_hsignal(
                'irc_redirection_ignore_filter_userhost',
                'cb_hsignal_irc_redirection_ignore_filter_userhost',
                key
            )
            w.hook_signal_send('irc_input_send', w.WEECHAT_HOOK_SIGNAL_STRING,
                               f'{server};;;;/userhost {args[1]}')
        else:
            ignore_filter_add(key, '^nick_{}$'.format(args[1]))
    return w.WEECHAT_RC_OK


def ignore_filter_cb_line(data, line):
    buffer = line['buffer']
    btype = w.buffer_get_string(buffer, 'localvar_type')
    if btype != 'channel':
        return {}
    bname = line['buffer_name']
    _, server, channel = bname.split('.', 2)
    server = server.rstrip('~')
    tags = set(line['tags'].split(','))
    self_msg = 'self_msg' in tags
    if not self_msg:
        nick = tag_by_prefix(tags, 'nick_') or ''
        host = tag_by_prefix(tags, 'host_') or ''
        for regex in itertools.chain(re_ignore_filter.get(server, []),
                                     re_ignore_filter.get(server + '.' + channel, [])):
            if regex.match('nick_' + nick) or regex.match('host_' + host):
                if not w.buffer_get_string(buffer, 'localvar_ignore_filter'):
                    w.prnt(buffer, '×\t')
                w.buffer_set(buffer, 'localvar_set_ignore_filter', '1')
                return {
                    'prefix': w.string_eval_expression('${color:white,red}×${color:reset}', {}, {}, {}) + line['prefix'],
                    'tags': line['tags'] + ',irc_ignore_filter',
                }
    w.buffer_set(buffer, 'localvar_del_ignore_filter', '')
    return {}


w.hook_command('ignore_filter', '', '', '',
               'toggle '
               ' || del * -all|-server|-channel'
               ' || host %(nick) -channel %-'
               ' || nick %(nick) -channel %-',
               'cb_command_ignore_filter', '')
w.hook_line('', 'irc.*', 'irc_privmsg', 'ignore_filter_cb_line', '')

cmd('alias add IFILTER ignore_filter')

key_bind('meta-i', '/ignore_filter toggle')

ignore_filter_reload()


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

key_bind('meta-ctrl-M', '/set_read')


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


# Relay servers
# ----------------------------------------

def connect_relay(server_name):
    cmd(f'server add {server_name}~ 172.31.1.1/9000 -temp -nossl'
        f' -password={server_name}:${{sec.data.relay}}')
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


# Save autojoins
# ----------------------------------------

def cb_signal_save_autojoins(data, signal, args):
    for server in servers_iter():
        server_name = w.hdata_string(hd_server, server, 'name')
        if w.hdata_integer(hd_server, server, 'temp_server') > 0:
            continue
        autojoin = []
        for channel in channels_iter(server):
            channel_name = w.hdata_string(hd_channel, channel, 'name')
            channel_type = w.hdata_integer(hd_channel, channel, 'type')
            if channel_type == 0:
                autojoin.append(channel_name)
        if autojoin:
            autojoin.sort()
            val = ','.join(autojoin)
            cmd(f'set irc.server.{server_name}.autojoin {val}')
    return w.WEECHAT_RC_OK


w.hook_signal('quit', 'cb_signal_save_autojoins', '')


# Strip commands for whitespace
# ----------------------------------------

def cb_command_run_strip_input(data, buffer, command):
    input_line = w.buffer_get_string(buffer, 'input')
    w.buffer_set(buffer, 'input', input_line.strip())
    return w.WEECHAT_RC_OK


w.hook_command_run('/input return', 'cb_command_run_strip_input', '')


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

bar_addreplace(
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
        'btn_wspace_next',
        'btn_wspace_prev',
        'btn_wspace_del',
        'btn_quit',
        'btn_close',
        'btn_clear',
        'btn_set_read_all',
    ])
)
bar_item_cmd_btn('btn_fn', '>>', '/bar toggle fn')

bar_addreplace(
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

key_bind('@item(buffer_name):button1', '/input switch_active_buffer', 'mouse')
key_bind('@item(buffer_short_name):button1', '/input switch_active_buffer', 'mouse')
key_bind('@item(mode_indicator):button1', '/vimode_go_to_normal', 'mouse')
key_bind('@item(scroll):button1', '/window scroll_bottom', 'mouse')


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

w.buffer_set(core, 'localvar_set_loaded_custom', '1')


# ----------------------------------------

def configure():
    cmd('set autosort.sorting.case_sensitive on')
    cmd('set autosort.v3.helpers {"core_first": "${if:${buffer.full_name}!=core.weechat}", "irc_raw_last": "${if:${buffer.full_name}==irc.irc_raw}", "irc_last": "${if:${buffer.plugin.name}==irc}", "hashless_name": "${info:autosort_replace,#,,${buffer.name}}", "irc_first": "${if:${buffer.plugin.name}!=irc}", "irc_raw_first": "${if:${buffer.full_name}!=irc.irc_raw}"}')
    cmd('set autosort.v3.rules ["${core_first}", "${irc_last}", "${buffer.plugin.name}", "${irc_raw_first}", "${if:${plugin}==irc?${if:${server}!~\\\\|*bitlbee}}", "${if:${plugin}==irc?${info:autosort_order,${type},server,private,*,channel}}", "${buffer.short_name}", "${buffer.full_name}"]')
    cmd('set buflist.format.buffer ${format_number}${indent}${color_hotlist}${if:${type}!=channel?${color:lightgreen}${if:${type}==private?${color:lightcyan}}}${cut:13,${color:white}+,${name}}${format_hotlist}')
    cmd('set buflist.format.buffer_current ${color:,237}${format_number}${indent}${color:white}${color:*white}${cut:13,${color:white}+,${name}}${format_hotlist}')
    cmd('set buflist.format.hotlist " ${hotlist}"')
    cmd('set buflist.format.hotlist_highlight ${color:lightred}')
    cmd('set buflist.format.hotlist_low ${color:default}')
    cmd('set buflist.format.hotlist_message ${color:yellow}')
    cmd('set buflist.format.hotlist_private ${color:yellow}')
    cmd('set buflist.format.indent ""')
    cmd('set buflist.format.number "${number} "')
    cmd('set buflist.look.mouse_move_buffer off')
    cmd('set colorize_nicks.look.colorize_input on')
    cmd('set fset.look.condition_catch_set ""')
    cmd('set irc.color.input_nick white')
    cmd('set irc.color.message_chghost default')
    cmd('set irc.color.message_join default')
    cmd('set irc.color.message_kick default')
    cmd('set irc.color.message_quit default')
    cmd('set irc.ctcp.clientinfo ""')
    cmd('set irc.ctcp.finger ""')
    cmd('set irc.ctcp.source ""')
    cmd('set irc.ctcp.time ""')
    cmd('set irc.ctcp.userinfo ""')
    cmd('set irc.ctcp.version ""')
    cmd('set irc.look.buffer_switch_autojoin off')
    cmd('set irc.look.highlight_server ""')
    cmd('set irc.look.pv_buffer merge_by_server')
    cmd('set irc.look.server_buffer independent')
    cmd('set irc.look.temporary_servers on')
    cmd('set irc.msgbuffer.whois current')
    cmd('set irc.network.lag_reconnect 0')
    cmd('set irc.server_default.autoconnect on')
    cmd('set irc.server_default.autorejoin on')
    cmd('set irc.server_default.autorejoin_delay 10')
    cmd('set irc.server_default.connection_timeout 300')
    cmd('set irc.server_default.ipv6 off')
    cmd('set irc.server_default.msg_part ""')
    cmd('set irc.server_default.msg_quit ""')
    cmd('set irc.server_default.nicks mkoskar')
    cmd('set irc.server_default.realname Miroslav Koškár <https://mkoskar.com/>')
    cmd('set irc.server_default.sasl_timeout 30')
    cmd('set irc.server_default.sasl_username mkoskar')
    cmd('set irc.server_default.ssl on')
    cmd('set irc.server_default.ssl_cert {}/ssl/nick.pem'.format(config_dir_ref))
    cmd('set irc.server_default.usermode +iwR')
    cmd('set irc.server_default.username mkoskar')
    cmd('set logger.level.core.weechat 0')
    cmd('set logger.level.irc 3')
    cmd('set logger.look.backlog 50')
    cmd('set plugins.var.perl.highmon.short_names on')
    cmd('set plugins.var.python.go.color_name default,234')
    cmd('set plugins.var.python.go.color_name_highlight *green,234')
    cmd('set plugins.var.python.go.color_name_highlight_selected black,15')
    cmd('set plugins.var.python.go.color_number *white,default')
    cmd('set plugins.var.python.go.color_number_selected *white,red')
    cmd('set plugins.var.python.go.short_name on')
    cmd('set plugins.var.python.go.short_name_server on')
    cmd('set plugins.var.python.vimode.mode_indicator_cmd_color_bg red')
    cmd('set plugins.var.python.vimode.mode_indicator_insert_color ""')
    cmd('set plugins.var.python.vimode.mode_indicator_insert_color_bg ""')
    cmd('set plugins.var.python.vimode.mode_indicator_normal_color_bg red')
    cmd('set plugins.var.python.vimode.mode_indicator_search_color_bg red')
    cmd('set plugins.var.python.vimode.no_warn on')
    cmd('set plugins.var.python.vimode.search_vim on')
    cmd('set relay.color.status_active lightblue')
    cmd('set relay.color.status_auth_failed lightred')
    cmd('set relay.color.status_connecting yellow')
    cmd('set relay.color.status_waiting_auth brown')
    cmd('set relay.look.auto_open_buffer off')
    cmd('set relay.network.bind_address 172.31.1.1')
    cmd('set relay.network.ipv6 off')
    cmd('set relay.network.max_clients 30')
    cmd('set relay.network.password ${sec.data.relay}')
    cmd('set relay.port.irc 9000')
    cmd('set relay.port.weechat 9001')
    cmd('set script.scripts.download_enabled on')
    cmd('set script.scripts.hold custom.py')
    cmd('set sec.crypt.passphrase_command cat ~/.secrets/weechat-passphrase')
    cmd('set sec.crypt.passphrase_file ~/.secrets/weechat-passphrase')
    cmd('set spell.check.default_dict en')
    cmd('set spell.check.enabled on')
    cmd('set spell.check.real_time on')
    cmd('set spell.check.suggestions 5')
    cmd('set weechat.color.chat_day_change 245')
    cmd('set weechat.color.chat_highlight_bg black')
    cmd('set weechat.color.chat_inactive_buffer 239')
    cmd('set weechat.color.chat_nick_colors 10,3,12,111,5,13,6,14')
    cmd('set weechat.color.chat_prefix_buffer default')
    cmd('set weechat.color.chat_prefix_buffer_inactive_buffer 239')
    cmd('set weechat.color.chat_prefix_more white')
    cmd('set weechat.color.chat_prefix_suffix 239')
    cmd('set weechat.color.chat_read_marker 237')
    cmd('set weechat.color.chat_tags lightred')
    cmd('set weechat.color.chat_text_found black')
    cmd('set weechat.color.chat_text_found_bg brown')
    cmd('set weechat.color.emphasized black')
    cmd('set weechat.color.emphasized_bg brown')
    cmd('set weechat.color.input_text_not_found lightred')
    cmd('set weechat.color.nicklist_away default')
    cmd('set weechat.color.separator 237')
    cmd('set weechat.color.status_count_highlight lightred')
    cmd('set weechat.color.status_count_msg yellow')
    cmd('set weechat.color.status_count_private yellow')
    cmd('set weechat.color.status_data_highlight lightred')
    cmd('set weechat.color.status_data_private lightcyan')
    cmd('set weechat.color.status_name yellow')
    cmd('set weechat.completion.nick_case_sensitive on')
    cmd('set weechat.completion.partial_completion_command on')
    cmd('set weechat.completion.partial_completion_command_arg on')
    cmd('set weechat.completion.partial_completion_templates ""')
    cmd('set weechat.look.buffer_notify_default message')
    cmd('set weechat.look.buffer_time_format %H:%M')
    cmd('set weechat.look.buffer_time_same " "')
    cmd('set weechat.look.color_inactive_time on')
    cmd('set weechat.look.color_inactive_window off')
    cmd('set weechat.look.hotlist_add_conditions 1')
    cmd('set weechat.look.hotlist_count_min_msg 1')
    cmd('set weechat.look.hotlist_names_count 100')
    cmd('set weechat.look.hotlist_names_length 11')
    cmd('set weechat.look.hotlist_names_level 15')
    cmd('set weechat.look.hotlist_prefix ""')
    cmd('set weechat.look.hotlist_remove buffer')
    cmd('set weechat.look.hotlist_update_on_buffer_switch off')
    cmd('set weechat.look.key_bind_safe off')
    cmd('set weechat.look.mouse on')
    cmd('set weechat.look.prefix_action *')
    cmd('set weechat.look.prefix_align_max 10')
    cmd('set weechat.look.prefix_align_min 8')
    cmd('set weechat.look.prefix_align_more_after off')
    cmd('set weechat.look.prefix_buffer_align left')
    cmd('set weechat.look.prefix_buffer_align_more_after off')
    cmd('set weechat.look.prefix_error !!')
    cmd('set weechat.look.prefix_join ->')
    cmd('set weechat.look.prefix_quit <-')
    cmd('set weechat.look.prefix_same_nick …')
    cmd('set weechat.look.prefix_suffix ""')
    cmd('set weechat.look.read_marker_always_show on')
    cmd('set weechat.look.read_marker_update_on_buffer_switch off')
    cmd('set weechat.look.scroll_page_percent 50')
    cmd('set weechat.look.window_separator_horizontal off')
    cmd('set weechat.look.window_title WeeChat ${info:version}')
    cmd('set weechat.plugin.autoload alias,buflist,charset,fset,irc,logger,perl,python,script,spell,trigger')
    cmd('set weechat.startup.display_logo off')
    cmd('set weechat.startup.display_version off')

    cmd('alias add A allserv /away')
    cmd('alias add AFK allserv /away afk')
    cmd('alias add ALIS msg alis')
    cmd('alias add B buffer')
    cmd('alias add C close')
    cmd('alias add CC window merge')
    cmd('alias add CLA buffer clear -all')
    cmd('alias add CSERV msg ChanServ')
    cmd('alias add G grep --hilight --buffer')
    cmd('alias add GG grep --hilight')
    cmd('alias add MEMOSERV msg MemoServ')
    cmd('alias add NSERV msg NickServ')
    cmd('alias add QA quit')
    cmd('alias add S server jump')
    cmd('alias add SP window splith')
    cmd('alias add TOGGLE_BUFLIST bar toggle buflist')
    cmd('alias add TOGGLE_NICKLIST eval /buffer set nicklist ${if:${buffer.nicklist}==0}')
    cmd('alias add TOGGLE_NUMBER bar toggle vi_line_numbers')
    cmd('alias add TOGGLE_PREFIX /eval -s /mute set weechat.look.prefix_align ${if:${weechat.look.prefix_align}!=none?none:right} \\; /mute set weechat.look.align_end_of_lines ${if:${weechat.look.prefix_align}==none?prefix:message}')
    cmd('alias add TOGGLE_TIME eval /buffer set time_for_each_line ${if:${buffer.time_for_each_line}==0}')
    cmd('alias add VS window splitv')
    cmd('alias add WC window merge')
    cmd('alias add WO window merge all')
    cmd('alias addcompletion %(buffers_plugins_names) MSGBUF command -buffer $1 * input send $2-')

    bar_addreplace(
        'input', 'off', '1000', 'root', '',
        'bottom', 'horizontal', 'vertical',
        '1', '0', 'default', 'default', 'default', 'default', 'off',
        '[mode_indicator],[input_prompt]+(away),scroll,[input_search],[input_paste],input_text'
    )

    bar_addreplace(
        'status', 'off', '500', 'root', '${info:term_width} >= 120',
        'bottom', 'horizontal', 'vertical',
        '0', '0', 'white', 'default', '237', '237', 'off',
        'btn_fn,ws1,ws2,ws3,ws4,ws5,ws6,ws7,ws8,ws9,btn_zoom,btn_filter,btn_scroll_unread,btn_set_read,buffer_number+:+buffer_short_name+(buffer_modes)+{btn_nicklist_count},[lag],completion,spell_suggest'
    )

    bar_addreplace(
        'status_c1', 'off', '499', 'root', '${info:term_width} < 120',
        'bottom', 'horizontal', 'vertical',
        '0', '0', 'white', 'default', '237', '237', 'off',
        'ws1,ws2,ws3,ws4,ws5,ws6,ws7,ws8,ws9,btn_zoom,btn_filter,btn_scroll_unread,btn_set_read,[lag]'
    )

    bar_addreplace(
        'status_c2', 'off', '498', 'root', '${info:term_width} >= 80 && ${info:term_width} < 120',
        'bottom', 'horizontal', 'vertical',
        '0', '0', 'white', 'default', '237', '237', 'off',
        'btn_fn,buffer_number+:+buffer_short_name+(buffer_modes)+{btn_nicklist_count},completion,spell_suggest'
    )

    bar_addreplace(
        'status_c3', 'off', '497', 'root', '${info:term_width} < 80',
        'bottom', 'horizontal', 'vertical',
        '0', '0', 'white', 'default', '237', '237', 'off',
        'btn_fn,buffer_number+:+buffer_short_name+{btn_nicklist_count},completion,spell_suggest'
    )

    bar_addreplace(
        'title', 'off', '500', 'window', '${active}',
        'top', 'horizontal', 'vertical',
        '1', '0', '*white', 'default', '237', '237', 'off',
        'btn_win_fn,buffer_name,btn_server'
    )

    bar_addreplace(
        'title_inactive', 'off', '500', 'window', '${inactive}',
        'top', 'horizontal', 'vertical',
        '1', '0', 'default', 'default', '237', '237', 'off',
        'btn_win_fn,buffer_name,btn_server'
    )

    cmd('buflist bar')
    cmd('bar set buflist filling_top_bottom horizontal')
    cmd('bar set buflist items buflist')
    cmd('bar set buflist size 24')

    cmd('bar set vi_line_numbers color_fg 244')
    cmd('bar set vi_line_numbers items line_numbers')

    cmd('filter addreplace ignore * irc_ignore_filter *')
    cmd('filter addreplace longlists *,!irc.server.* irc_367,irc_728 *')
    cmd('filter addreplace nick_root irc.bitlbee.* nick_root *')
    cmd('filter addreplace smart * irc_smart_filter *')

    cmd(
        """
        trigger addreplace last_nick print
            irc.*;notify_message
            "${type} == channel && ${tg_displayed} && ${tg_tag_nick} != ${nick}"
            ""
            "/buffer set localvar_set_last_nick ${tg_tag_nick}"
        """
    )
    cmd(
        """
        trigger addreplace last_nick_complete command_run
            "/input complete_next"
            "${type} == channel && ${buffer.input_buffer_length} == 0 && ${last_nick}"
            ""
            "/input insert ${last_nick}"
        """
    )
    cmd(
        """
        trigger addreplace quote modifier
            weechat_print
            "${tg_message_nocolor} =~ ^>[^:._]"
            "/.*/${tg_prefix}\\t${color:10}${tg_message}"
        """
    )
    cmd(
        """
        trigger addreplace url_color modifier
            weechat_print
            "${tg_tags} !!- ,irc_quit,"
            ";[[:alpha:]]+://[^[:blank:]]+;${color:yellow}${re:0}${color:reset}"
        """
    )

    key_bind("meta-'", '/input jump_smart')
    key_bind('ctrl-G', '/window scroll_bottom')
    key_bind('ctrl-M', '/input return ; /window scroll_bottom')
    key_bind('ctrl-N', '/input history_next')
    key_bind('ctrl-O', '/go')
    key_bind('ctrl-P', '/input history_previous')
    key_bind('meta-,', '/buffer_hot_prev')
    key_bind('meta-.', '/buffer_hot_next')
    key_bind('meta-;meta-N', '/TOGGLE_NUMBER')
    key_bind('meta-;meta-b', '/TOGGLE_BUFLIST')
    key_bind('meta-;meta-m', '/mute mouse toggle')
    key_bind('meta-;meta-n', '/TOGGLE_NICKLIST')
    key_bind('meta-;meta-p', '/TOGGLE_PREFIX')
    key_bind('meta-;meta-t', '/TOGGLE_TIME')
    key_bind('meta-G', '/window scroll_bottom')
    key_bind('meta-J', '/window scroll_down')
    key_bind('meta-K', '/window scroll_up')
    key_bind('meta-N', '/buffer +1')
    key_bind('meta-P', '/buffer -1')
    key_bind('meta-g', '/window scroll_top')
    key_bind('meta-jmeta-b', '/window bare')
    key_bind('meta-jmeta-j', '/cursor go chat')
    key_bind('meta-jmeta-n', '/cursor go nicklist')
    key_bind('meta-jmeta-u', '/urls_open')
    key_bind('meta-jmeta-y', '/urls_yank')
    key_bind('meta-m', '/window zoom')
    key_bind('meta-s', '/server jump')
    key_bind('meta-wmeta-h', '/window left')
    key_bind('meta-wmeta-j', '/window down')
    key_bind('meta-wmeta-k', '/window up')
    key_bind('meta-wmeta-l', '/window right')

    key_bind('ctrl-N', '/input search_next', 'search')
    key_bind('ctrl-P', '/input search_previous', 'search')

    key_bind('h', '/cursor move left', 'cursor')
    key_bind('j', '/cursor move down', 'cursor')
    key_bind('k', '/cursor move up', 'cursor')
    key_bind('l', '/cursor move right', 'cursor')
    key_bind('meta-h', '/cursor move area_left', 'cursor')
    key_bind('meta-j', '/cursor move area_down', 'cursor')
    key_bind('meta-k', '/cursor move area_up', 'cursor')
    key_bind('meta-l', '/cursor move area_right', 'cursor')


def cb_command_configure(data, buffer, args):
    configure()
    return w.WEECHAT_RC_OK


def cb_command_reconfigure(data, buffer, args):
    cmd('disconnect -all')
    while True:
        time.sleep(0.1)
        for server in servers_iter():
            if w.hdata_integer(hd_server, server, 'is_connected') > 0:
                break
        else:
            break

    autojoins = {}
    for server in list(servers_iter()):
        server_name = w.hdata_string(hd_server, server, 'name')
        autojoin = w.config_string(server_opt(server_name, 'autojoin'))
        autojoins[server_name] = autojoin
        cmd(f'server del {server_name}')

    plugins_vars = {}
    for opt in options_iter(w.config_search_section(config_file_search('plugins'), 'var')):
        opt_name = w.hdata_string(hd_config_option, opt, 'name')
        key, _, subkey = opt_name.partition('.')
        if key == 'ignore_filter':
            plugins_vars[opt_name] = w.config_string(opt)

    for config_file in config_files_iter():
        config_name = w.hdata_string(hd_config_file, config_file, 'name')
        if config_name == 'alias':
            continue
        cmd(f'unset -mask {config_name}.*')

    cmd('bar del -all')
    cmd('bar default')
    cmd('filter del -all')
    cmd('ignore del -all')
    cmd('trigger default -yes')
    cmd('key resetall -yes')

    configure()

    cmd('proxy del default')
    cmd('proxy add default socks5 127.0.0.1 1081')
    cmd('proxy del tor')
    cmd('proxy add tor socks5 127.0.0.1 9050')

    cmd('server add bitlbee localhost')
    cmd('set irc.server.bitlbee.nicks miro')
    cmd('set irc.server.bitlbee.proxy ""')
    cmd('set irc.server.bitlbee.sasl_password ${sec.data.bitlbee}')
    cmd('set irc.server.bitlbee.sasl_username miro')
    cmd('set irc.server.bitlbee.ssl off')
    cmd('set irc.server.bitlbee.usermode ""')

    cmd('server add gitter irc.gitter.im/6697')
    cmd('set irc.server.gitter.autoconnect off')
    cmd('set irc.server.gitter.password ${sec.data.gitter}')

    cmd('server add gnome irc.gnome.org/6697')
    cmd('set irc.server.gnome.sasl_password ${sec.data.gnome}')

    cmd('server add libera irc.libera.chat/6697')
    cmd('set irc.server.libera.sasl_password ${sec.data.libera}')

    cmd('server add libera-alt irc.libera.chat/6697')
    cmd('set irc.server.libera-alt.nicks miro')
    cmd('set irc.server.libera-alt.realname miro')
    cmd('set irc.server.libera-alt.sasl_password ${sec.data.libera}')
    cmd('set irc.server.libera-alt.username miro')

    cmd('server add oftc irc.oftc.net/6697')

    for key, val in autojoins.items():
        cmd(f'set irc.server.{key}.autojoin "{val}"')

    for var in plugins_vars.keys():
        cmd(f'set plugins.var.{var} "{plugins_vars[var]}"')

    w.command_options(core, '/upgrade', {'delay': '10'})
    return w.WEECHAT_RC_OK


w.hook_command('configure', '', '', '', 'all', 'cb_command_configure', '')
w.hook_command('reconfigure', '', '', '', 'all', 'cb_command_reconfigure', '')
