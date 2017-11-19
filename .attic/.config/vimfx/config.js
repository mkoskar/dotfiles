Components.utils.import(`${__dirname}/common.js`)

vimfx.set('ignore_keyboard_layout', true)
vimfx.set('notify_entered_keys', true)

vimfx.set('mode.caret.exit', '<escape> <c-[> q Q')
vimfx.set('mode.find.exit', '<escape> <enter> <c-[>')
vimfx.set('mode.hints.exit', '<escape> <c-[>')
vimfx.set('mode.marks.exit', '<escape> <c-[>')
vimfx.set('mode.normal.element_text_caret', 'v <c-]>')
vimfx.set('mode.normal.enter_mode_ignore', '<c-z>')
vimfx.set('mode.normal.find_highlight_all', 'a/ <c-space>')
vimfx.set('mode.normal.focus_search_bar', '')
vimfx.set('mode.normal.history_back', 'H <force><c-o>')
vimfx.set('mode.normal.history_forward', 'L <force><c-i>')
vimfx.set('mode.normal.quote', '<force><c-v>')
vimfx.set('mode.normal.scroll_half_page_down', '<c-d>')
vimfx.set('mode.normal.scroll_half_page_up', '<c-u>')
vimfx.set('mode.normal.scroll_page_down', '')
vimfx.set('mode.normal.scroll_page_up', '')
vimfx.set('mode.normal.stop', 's <force><c-c>')
vimfx.set('mode.normal.tab_close', 'x d')
vimfx.set('mode.normal.tab_close_other', 'gxa gda')
vimfx.set('mode.normal.tab_close_to_end', 'gx$ gd$')
vimfx.set('mode.normal.tab_move_backward', 'gJ')
vimfx.set('mode.normal.tab_move_forward', 'gK')
vimfx.set('mode.normal.tab_new', 't <a-n>')
vimfx.set('mode.normal.tab_restore', 'X u')
vimfx.set('mode.normal.tab_restore_list', 'gX U')
vimfx.set('mode.normal.tab_select_last', '<a-0>')
vimfx.set('mode.normal.tab_select_next', 'K gt')
vimfx.set('mode.normal.tab_select_previous', 'J gT')

// ----------------------------------------

const SCROLLX_FACTOR = 3

vimfx.addCommand({
    name: 'scroll_down_x',
    description: 'Scroll down X',
}, (args) => {
    args.count = args.count > 0 ? args.count : 1
    args.count *= SCROLLX_FACTOR
    vimfx.modes.normal.commands.scroll_down.run(args)
})
vimfx.set('custom.mode.normal.scroll_down_x', '<a-j>')

vimfx.addCommand({
    name: 'scroll_up_x',
    description: 'Scroll up X',
}, (args) => {
    args.count = args.count > 0 ? args.count : 1
    args.count *= SCROLLX_FACTOR
    vimfx.modes.normal.commands.scroll_up.run(args)
})
vimfx.set('custom.mode.normal.scroll_up_x', '<a-k>')

// ----------------------------------------

vimfx.addCommand({
    name: 'zoom_in',
    description: 'Zoom in',
}, (args) => {
    args.vim.window.FullZoom.enlarge()
})
vimfx.set('custom.mode.normal.zoom_in', 'zi')

vimfx.addCommand({
    name: 'zoom_out',
    description: 'Zoom out',
}, (args) => {
    args.vim.window.FullZoom.reduce()
})
vimfx.set('custom.mode.normal.zoom_out', 'zo')

vimfx.addCommand({
    name: 'zoom_reset',
    description: 'Zoom reset',
}, (args) => {
    args.vim.window.FullZoom.reset()
})
vimfx.set('custom.mode.normal.zoom_reset', 'zz')

// ----------------------------------------

vimfx.addCommand({
    name: 'esc_alt',
    description: 'esc_alt',
}, (args) => {
    let {event, vim} = args, win = vim.window, el = event.target
    event.stopPropagation()
    el.dispatchEvent(new win.KeyboardEvent('keypress', {keyCode: 27}))
    el.dispatchEvent(new win.KeyboardEvent('keypress', {keyCode: 27}))
    el.blur()
    vimfx.modes.normal.commands.esc.run(args)
})
vimfx.set('custom.mode.normal.esc_alt', '<force><c-[>')

function addInputCommand(name, description, mappings) {
    for (mode of ['normal', 'find']) {
        vimfx.addCommand({
            name: name,
            description: description,
            mode: mode,
        }, (args) => {
            let {event, vim} = args, win = vim.window
            let el = findActiveElement(win.document)
            if (vim.focusType.match(/editable|findbar/)) {
                event.stopPropagation()
                if (el.tagName.match(/(xul:)?browser/i)) {
                    vimfx.send(vim, name)
                } else {
                    inputHandlers[name](win, el)
                }
            }
        })
        vimfx.set(`custom.mode.${mode}.${name}`, mappings)
    }
}

addInputCommand('input_home', 'Input: home', '<force><c-b>')
addInputCommand('input_end', 'Input: end', '<force><c-e>')
addInputCommand('input_left', 'Input: left', '<force><a-h>')
addInputCommand('input_right', 'Input: right', '<force><a-l>')
addInputCommand('input_wleft', 'Input: word left', '<force><a-b>')
addInputCommand('input_wright', 'Input: word right', '<force><a-f> <force><a-w>')

addInputCommand('input_kleft', 'Input: kill left', '<force><c-u> <force><a-u>') // <c-u> doesn't work
addInputCommand('input_kright', 'Input: kill right', '<force><c-k>')
addInputCommand('input_werase', 'Input: word erase', '<force><c-w>')
addInputCommand('input_wdelete', 'Input: word delete', '<force><a-d>')
addInputCommand('input_delete', 'Input: delete', '<force><a-x>')

addInputCommand('input_up', 'Input: up', '<force><c-p>')
addInputCommand('input_down', 'Input: down', '<force><c-n>')
addInputCommand('input_esc', 'Input: escape', '<force><c-x>')
addInputCommand('input_erase', 'Input: erase', '<force><c-h>')

addInputCommand('input_home_sel', 'Input: select home', '<force><a-(>')
addInputCommand('input_end_sel', 'Input: select end', '<force><a-)>')
addInputCommand('input_left_sel', 'Input: select left', '<force><a-H>')
addInputCommand('input_right_sel', 'Input: select right', '<force><a-L>')
addInputCommand('input_wleft_sel', 'Input: select word left', '<force><a-B>')
addInputCommand('input_wright_sel', 'Input: select word right', '<force><a-F> <force><a-W>')

// ----------------------------------------

function noop(args) {
    args.event.stopPropagation()
}

vimfx.addCommand({name: 'noop', description: 'NOOP', mode: 'caret'}, noop)
vimfx.addCommand({name: 'noop', description: 'NOOP', mode: 'find'}, noop)
vimfx.addCommand({name: 'noop', description: 'NOOP', mode: 'hints'}, noop)
vimfx.addCommand({name: 'noop', description: 'NOOP', mode: 'marks'}, noop)
vimfx.addCommand({name: 'noop', description: 'NOOP', mode: 'normal'}, noop)

vimfx.set('custom.mode.caret.noop', '<force><c-w>')
vimfx.set('custom.mode.find.noop', '<force><c-w>')
vimfx.set('custom.mode.hints.noop', '<force><c-w>')
vimfx.set('custom.mode.marks.noop', '<force><c-w>')
