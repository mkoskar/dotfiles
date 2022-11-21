local msg = require('mp.msg')
local options = require('mp.options')
local utils = require('mp.utils')

local info = ''
local info_enabled = false

local chapter_go_pos = -1
local chapter_go_timer = nil
local playlist_go_pos = -1
local playlist_go_timer = nil

local input_lock = false
local input_keys = {
    '!',
    '@',
    'E',
    'L',
    'r',
    's',
    'ENTER',
    'Ctrl+L',
    ',', '.',
    'g', 'G',
    'j', 'Alt+j', 'Ctrl+j',
    'k', 'Alt+k', 'Ctrl+k',
    'h', 'Alt+h', 'Ctrl+h',
    'l', 'Alt+l', 'Ctrl+l',
    'UP', 'Shift+UP',
    'DOWN', 'Shift+DOWN',
    'LEFT', 'Shift+LEFT', 'Ctrl+LEFT',
    'RIGHT', 'Shift+RIGHT', 'Ctrl+RIGHT',
    'WHEEL_UP', 'WHEEL_DOWN',
    'PGUP', 'Shift+PGUP',
    'PGDWN', 'Shift+PGDWN',
    'Shift+BS', 'Shift+Ctrl+BS',
    'FORWARD', 'REWIND',
    '[', ']', '{', '}',
    'NEXT', 'PREV', '<', '>', 'MBTN_BACK', 'MBTN_FORWARD',
    'Alt+p', 'Alt+n', 'Alt+P', 'Alt+N', 'Alt+g', 'Alt+G',
    'Ctrl+p', 'Ctrl+n', 'Ctrl+P', 'Ctrl+N', 'Ctrl+g', 'Ctrl+G',
    'q', 'Q', 'Ctrl+c', 'Ctrl+w', 'CLOSE_WIN', 'STOP', 'POWER',
}

function show_info()
    if info_enabled then
        return
    end
    mp.osd_message(info)
end

function toggle_info()
    info_enabled = not info_enabled
    set_info()
end

function toggle_input_lock()
    for i, k in ipairs(input_keys) do
        if input_lock then
            mp.remove_key_binding('input_lock' .. i)
        else
            mp.add_forced_key_binding(k, 'input_lock' .. i, ignore)
        end
    end
    input_lock = not input_lock
    mp.osd_message(input_lock and "Input locked" or "Input unlocked")
end

function set_info()
    local title = format_title()
    local chapter = format_chapter()
    info = title .. '\n'
    if chapter then
        info = info .. chapter .. '\n'
    end
    local info_ = ''
    if info_enabled then
        info_ = info
    end
    mp.set_property('options/osd-msg1', info_)
    mp.set_property(
        'options/osd-msg2', info_ ..
        '\n${osd-sym-cc} ${time-pos:-} / ${time-remaining:-} / ${percent-pos:-}${?percent-pos:%}\n' ..
        'audio: ${audio-codec-name} / ${audio-params/format} / ${audio-params/samplerate}\n' ..
        'video: ${video-format} / ${video-params/pixelformat} / ${video-params/w}x${video-params/h}\n' ..
        'cache-buffering-state [%]: ${cache-buffering-state:-}\n' ..
        'cache-used: ${cache-used:-}\n' ..
        'demuxer-cache-duration [s]: ${demuxer-cache-duration:-}\n' ..
        'display-fps: ${display-fps}\n'
    )
    mp.set_property(
        'options/osd-msg3', info_ ..
        '\n${osd-sym-cc} ${time-pos:-} / ${time-remaining:-} / ${percent-pos:-}${?percent-pos:%}\n'
    )
    mp.osd_message('')
end

function format_title()
    return string.format(
        '[%d/%d] %s',
        mp.get_property_number('playlist-pos-1', 0),
        mp.get_property_number('playlist-count', 0),
        mp.get_property('media-title')
    )
end

function format_chapter()
    local pos = mp.get_property_number('chapter', -1)
    if pos < 0 then
        return nil
    end
    local props = mp.get_property_native('chapter-metadata', {})
    local title = props['title'] or 'n/a'
    return string.format('(%d) %s', pos + 1, title)
end

function limited_list(prop, pos)
    local props = mp.get_property_native(prop, {})
    local count = #props
    if count == 0 then
        return count, props
    end
    local delta = 5
    local max = 2 * delta + 1
    local begi = math.max(math.min(pos - delta, count - max + 1), 1)
    local endi = math.min(begi + max - 1, count)
    local reslist = {}
    for i=begi, endi do
        local item = props[i]
        item.current = (i == pos) and true or nil
        table.insert(reslist, item)
    end
    return count, reslist
end

function show_chapters()
    local pos = chapter_go_pos
    if pos < 0 then
        pos = mp.get_property_number('chapter', -1)
    end
    local count, limlist = limited_list('chapter-list', pos + 1)
    local msg = count == 0 and
        'No chapters.' or
        string.format('Chapters [%d/%d]:\n', pos + 1, count)
    for i, v in ipairs(limlist) do
        local title = v.title
        if title and #title > 60 then
            title = title:sub(1, 60) .. '...'
        end
        msg = string.format('%s %s %s\n', msg,
                            (v.current and '●' or '○'), title)
    end
    if not info_enabled then
        msg = info .. '\n' .. msg
    end
    mp.osd_message(msg, 3.5)
end

function show_playlist()
    local pos = playlist_go_pos
    if pos < 0 then
        pos = mp.get_property_number('playlist-pos', -1)
    end
    local count, limlist = limited_list('playlist', pos + 1)
    local msg = count == 0 and 'Empty playlist.' or
        string.format('Playlist [%d/%d]:\n', pos + 1, count)
    for i, v in ipairs(limlist) do
        local title = v.title
        local _, filename = utils.split_path(v.filename)
        if title == nil then
            title = filename
        end
        if title and #title > 60 then
            title = title:sub(1, 60) .. '...'
        end
        msg = string.format('%s %s %s\n', msg,
            (v.current and '●' or '○'), title)
    end
    if not info_enabled then
        msg = info .. '\n' .. msg
    end
    mp.osd_message(msg, 3.5)
end

function chapter_go(target)
    local last = mp.get_property_number('chapters', 0) - 1
    if last < 0 then
        return
    end
    local pos = chapter_go_pos
    if pos < 0 then
        pos = mp.get_property_number('chapter', -1)
    end
    if target == 'first' then
        chapter_go_pos = 0
    elseif target == 'last' then
        chapter_go_pos = last
    else
        chapter_go_pos = math.max(math.min(pos + target, last), 0)
    end
    if chapter_go_timer then
        chapter_go_timer:kill()
        chapter_go_timer:resume()
    else
        chapter_go_timer = mp.add_timeout(0.5, function()
            local cur = mp.get_property_number('chapter', -1)
            if chapter_go_pos ~= cur then
                mp.set_property('chapter', chapter_go_pos)
            end
            chapter_go_pos = -1
        end)
    end
    show_chapters()
end

function playlist_go(target)
    local last = mp.get_property_number('playlist-count', 0) - 1
    if last < 0 then
        return
    end
    local pos = playlist_go_pos
    if pos < 0 then
        pos = mp.get_property_number('playlist-pos', -1)
    end
    if target == 'first' then
        playlist_go_pos = 0
    elseif target == 'last' then
        playlist_go_pos = last
    else
        playlist_go_pos = math.max(math.min(pos + target, last), 0)
    end
    if playlist_go_timer then
        playlist_go_timer:kill()
        playlist_go_timer:resume()
    else
        playlist_go_timer = mp.add_timeout(0.5, function()
            local cur = mp.get_property_number('playlist-pos', -1)
            if playlist_go_pos ~= cur then
                mp.set_property('playlist-pos', playlist_go_pos)
            end
            playlist_go_pos = -1
        end)
    end
    show_playlist()
end

function playlist_reverse()
    local count = mp.get_property_number('playlist-count', 0)
    for i=count, 1, -1 do
        mp.commandv('playlist-move', 0, i)
    end
    show_playlist()
end

function yank_property(name)
    local val = mp.get_property(name)
    if not val then
        return
    end
    if name == 'path' then
        val = val:gsub('^ytdl://', 'https://www.youtube.com/watch?v=')
    end
    local proc = io.popen('clip -i', 'w')
    pcall(proc:write(val))
    proc:close()
    mp.osd_message(val .. ' (yanked)')
end

function reload()
    local path = mp.get_property('path')
    if path then
        mp.commandv('loadfile', path, 'replace',
                    'start=' .. mp.get_property_number('time-pos'))
    end
end

mp.add_key_binding('i', 'show-info', show_info)
mp.add_key_binding('I', 'toggle-info', toggle_info)
mp.add_key_binding('p', 'show-playlist', show_playlist)
mp.add_key_binding('P', 'show-chapters', show_chapters)
mp.add_key_binding(nil, 'chapter-go', chapter_go)
mp.add_key_binding(nil, 'playlist-go', playlist_go)
mp.add_key_binding(nil, 'playlist-reverse', playlist_reverse)
mp.add_key_binding('y', 'yank-path', function() yank_property('path') end)
mp.add_key_binding('Y', 'yank-title', function() yank_property('media-title') end)
mp.add_key_binding('~', 'toggle-input-lock', toggle_input_lock)
mp.add_key_binding('Ctrl+r', 'reload', reload)

mp.register_event('file-loaded',
    function()
        set_info()
        show_info()
        local title = format_title()
        os.execute(string.format("notify '%s'", title:gsub("'", "'\\''")))
    end
)

mp.observe_property('chapter', 'number',
    function(name, value)
        local chapter = format_chapter()
        if not chapter then
            return
        end
        set_info()
        show_info()
        os.execute(string.format("notify '%s'", chapter:gsub("'", "'\\''")))
    end
)
