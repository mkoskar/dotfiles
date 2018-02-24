local utils = require('mp.utils')
local ts_chapter_go = -1
local ts_playlist_go = -1

function show_info()
    local title = format_title()
    local chapter = format_chapter()
    mp.osd_message(title .. '\n' .. (chapter or ''))
end

function format_title()
    return string.format(
        '[%d/%d] %s',
        mp.get_property_number('playlist-pos') + 1,
        mp.get_property_number('playlist-count'),
        mp.get_property('media-title')
    )
end

function format_chapter()
    local pos = mp.get_property_number('chapter', -1) + 1
    if pos > 0 then
        local props = mp.get_property_native('chapter-metadata', {})
        local title = props['title'] or 'n/a'
        return string.format('(%d) %s', pos, title)
    end
    return nil
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

function show_playlist()
    local pos = mp.get_property_number('playlist-pos', -1) + 1
    local count, limlist = limited_list('playlist', pos)
    local msg = count == 0 and 'Empty playlist.' or
        string.format('Playlist [%d/%d]:\n', pos, count)
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
    mp.osd_message(msg, 3.5)
end

function show_chapters()
    local pos = mp.get_property_number('chapter', -1) + 1
    local count, limlist = limited_list('chapter-list', pos)
    local msg = count == 0 and
        'No chapters.' or
        string.format('Chapters [%d/%d]:\n', pos, count)
    for i, v in ipairs(limlist) do
        local title = v.title
        if title and #title > 60 then
            title = title:sub(1, 60) .. '...'
        end
        msg = string.format('%s %s %s\n', msg,
            (v.current and '●' or '○'), title)
    end
    mp.osd_message(msg, 3.5)
end

function chapter_go(target)
    local last = mp.get_property_number('chapters', 0) - 1
    if last < 0 then
        return
    end
    local old = mp.get_property_number('chapter')
    local new
    if target == 'first' then
        new = 0
    elseif target == 'last' then
        new = last
    else
        new = math.max(math.min(old + target, last), 0)
    end
    if new ~= old then
        mp.set_property('chapter', new)
        ts_chapter_go = os.time()
        show_chapters()
    end
end

function playlist_go(target)
    local last = mp.get_property_number('playlist-count', 0) - 1
    if last < 0 then
        return
    end
    local old = mp.get_property_number('playlist-pos')
    local new
    if target == 'first' then
        new = 0
    elseif target == 'last' then
        new = last
    else
        new = math.max(math.min(old + target, last), 0)
    end
    if new ~= old then
        mp.set_property('playlist-pos', new)
        ts_playlist_go = os.time()
        show_playlist()
    end
end

function yank_property(name)
    local val = mp.get_property(name)
    if name == 'path' then
        val = val:gsub('^ytdl://', 'https://www.youtube.com/watch?v=')
    end
    local proc = io.popen('clip -i', 'w')
    pcall(proc:write(val))
    proc:close()
    mp.osd_message(val)
end

mp.add_key_binding(nil, 'show-info', show_info)
mp.add_key_binding(nil, 'show-chapters', show_chapters)
mp.add_key_binding(nil, 'show-playlist', show_playlist)
mp.add_key_binding(nil, 'chapter-go', chapter_go)
mp.add_key_binding(nil, 'playlist-go', playlist_go)
mp.add_key_binding(nil, 'yank-path', function () yank_property('path') end)
mp.add_key_binding(nil, 'yank-title', function () yank_property('media-title') end)

mp.set_property(
    'options/osd-msg2',
    '${osd-sym-cc} ${time-pos:-} / ${time-remaining:-} / ${percent-pos:-}${?percent-pos:%}\n' ..
    'audio: ${audio-codec-name} / ${audio-params/format} / ${audio-params/samplerate}\n' ..
    'video: ${video-format} / ${video-params/pixelformat} / ${video-params/w}x${video-params/h}\n' ..
    'cache-buffering-state [%]: ${cache-buffering-state:-}\n' ..
    'cache-used: ${cache-used:-}\n' ..
    'demuxer-cache-duration [s]: ${demuxer-cache-duration:-}\n' ..
    'display-fps: ${display-fps}\n\n'
)

mp.set_property(
    'options/osd-msg3',
    '${osd-sym-cc} ${time-pos:-} / ${time-remaining:-} / ${percent-pos:-}${?percent-pos:%}'
)

mp.register_event('file-loaded',
    function ()
        local now = os.time()
        if os.difftime(now, ts_playlist_go) < 2 then
            return
        end
        show_info()
        local title = format_title()
        os.execute(string.format("notify '%s'", title:gsub("'", "'\\''")))
    end
)

mp.observe_property('chapter', 'number',
    function (name, value)
        if not value or value < 0 then
            return
        end
        local now = os.time()
        if os.difftime(ts_chapter_go, ts_playlist_go) > 0 then
            if os.difftime(now, ts_chapter_go) < 2 then
                return
            end
            show_info()
        else
            if os.difftime(now, ts_playlist_go) > 2 then
                show_info()
            end
        end
        local chapter = format_chapter()
        os.execute(string.format("notify '%s'", chapter:gsub("'", "'\\''")))
    end
)
