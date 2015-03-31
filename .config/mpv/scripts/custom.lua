function show_info()
    mp.osd_message('[' .. mp.get_property('playlist-pos')+1 .. '/' .. mp.get_property('playlist-count') .. '] ' .. mp.get_property('media-title'))
end

mp.add_key_binding('i', 'show_info', show_info)
