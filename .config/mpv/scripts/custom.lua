function show_info()
    mp.osd_message(
        '[' .. mp.get_property('playlist-pos') + 1 ..
        '/' .. mp.get_property('playlist-count') ..
        '] ' .. mp.get_property('media-title')
    )
end

function yank_path()
    local file = io.popen('clipi', 'w')
    local path = mp.get_property('path')
    file:write(path)
    file:close()
    mp.osd_message(path)
end

mp.add_key_binding('i', 'show_info', show_info)
mp.add_key_binding('y', 'yank_path', yank_path)

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
