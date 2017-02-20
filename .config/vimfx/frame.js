Components.utils.import(`${__dirname}/common.js`)

function addInputCommand(name) {
    vimfx.listen(name, (data, cb) => {
        let el = findActiveElement(content.document)
        if (el && el.tagName.match(/(input|textarea)/i)) {
            inputHandlers[name](content, el)
        }
    })
}

addInputCommand('input_delete')
addInputCommand('input_down')
addInputCommand('input_end')
addInputCommand('input_end_sel')
addInputCommand('input_erase')
addInputCommand('input_esc')
addInputCommand('input_home')
addInputCommand('input_home_sel')
addInputCommand('input_kleft')
addInputCommand('input_kright')
addInputCommand('input_left')
addInputCommand('input_left_sel')
addInputCommand('input_right')
addInputCommand('input_right_sel')
addInputCommand('input_up')
addInputCommand('input_wdelete')
addInputCommand('input_werase')
addInputCommand('input_wleft')
addInputCommand('input_wleft_sel')
addInputCommand('input_wright')
addInputCommand('input_wright_sel')
