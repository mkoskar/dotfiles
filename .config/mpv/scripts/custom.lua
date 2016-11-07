local utils = require 'mp.utils'

function show_info()
    mp.osd_message(
        '[' .. mp.get_property('playlist-pos') + 1 ..
        '/' .. mp.get_property('playlist-count') ..
        '] ' .. mp.get_property('media-title')
    )
end

function limited_list(prop, pos)
    local proplist = mp.get_property_native(prop, {})
    local count = #proplist
    if count == 0 then
        return count, proplist
    end
    local delta = 5
    local max = 2 * delta + 1
    local begi = math.max(math.min(pos - delta, count - max + 1), 1)
    local endi = math.min(begi + max - 1, count)
    local reslist = {}
    for i=begi, endi do
        local item = proplist[i]
        item.current = (i == pos) and true or nil
        table.insert(reslist, item)
    end
    return count, reslist
end

function show_playlist()
    local pos = mp.get_property_number('playlist-pos') + 1
    local count, limlist = limited_list('playlist', pos)
    if count == 0 then
        return 'Empty playlist.'
    end
    local message = string.format('Playlist [%d/%d]:\n', pos, count)
    for i, v in ipairs(limlist) do
        local title = v.title
        local _, filename = utils.split_path(v.filename)
        if title == nil then
            title = filename
        end
        if title and #title > 60 then
            title = title:sub(1, 60) .. '...'
        end
        message = string.format('%s %s %s\n', message,
            (v.current and '●' or '○'), title)
    end
    mp.osd_message(message, 3.5)
end

function yank_property(name)
    local val = mp.get_property(name)
    if name == 'path' then
        val = val:gsub('^ytdl://', 'https://www.youtube.com/watch?v=')
    end
    local out = io.popen('clipi', 'w')
    pcall(out:write(val))
    out:close()
    mp.osd_message(val)
end

mp.add_key_binding('i', 'show-info', show_info)
mp.add_key_binding('F8', 'show-playlist', show_playlist)
mp.add_key_binding('y', 'yank-path', function () yank_property('path') end)
mp.add_key_binding('Y', 'yank-title', function () yank_property('media-title') end)

mp.set_property(
    'options/osd-msg2',
    '${osd-sym-cc} ${time-pos:-} / ${time-remaining:-} / ${length:-} (${percent-pos:-}%)\n' ..
    'FPS: ${fps} / ${display-fps}\n' ..
    'Cache: ${cache-buffering-state:-}% / ${demuxer-cache-duration:-} s / ${cache-used:-}\n' ..
    'Cache Size: ${cache-size:-} (${cache:-}%)'
)

mp.set_property(
    'options/osd-msg3',
    '${osd-sym-cc} ${time-pos:-} / ${time-remaining:-} / ${length:-} (${percent-pos:-}%)'
)

mp.register_event(
    'file-loaded',
    function ()
        show_info()
        local title = mp.get_property('media-title')
        os.execute("notify '" .. title:gsub("'", "'\\''") .. "'")
    end
)
