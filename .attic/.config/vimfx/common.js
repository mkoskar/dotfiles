const EXPORTED_SYMBOLS = [
    'findActiveElement',
    'inputHandlers',
]

function findActiveElement(document) {
    let inner = (document) => {
        let active = document.activeElement
        return active.contentDocument ?
            inner(active.contentDocument) :
            active
    }
    return inner(document)
}

let inputHandlers = {
    input_delete: (win, el) => {
        el.dispatchEvent(new win.KeyboardEvent('keypress', {
            bubbles: true, cancelable: true, keyCode: 46
        }))
    },

    input_down: (win, el) => {
        el.dispatchEvent(new win.KeyboardEvent('keypress', {
            bubbles: true, cancelable: true, keyCode: 40
        }))
    },

    input_end: (win, el) => {
        el.dispatchEvent(new win.KeyboardEvent('keypress', {
            bubbles: true, cancelable: true, keyCode: 35
        }))
    },

    input_end_sel: (win, el) => {
        el.dispatchEvent(new win.KeyboardEvent('keypress', {
            bubbles: true, cancelable: true, shiftKey: true, keyCode: 35
        }))
    },

    input_erase: (win, el) => {
        el.dispatchEvent(new win.KeyboardEvent('keypress', {
            bubbles: true, cancelable: true, keyCode: 8
        }))
    },

    input_esc: (win, el) => {
        el.dispatchEvent(new win.KeyboardEvent('keypress', {
            bubbles: true, cancelable: true, keyCode: 27
        }))
    },

    input_home: (win, el) => {
        el.dispatchEvent(new win.KeyboardEvent('keypress', {
            bubbles: true, cancelable: true, keyCode: 36
        }))
    },

    input_home_sel: (win, el) => {
        el.dispatchEvent(new win.KeyboardEvent('keypress', {
            bubbles: true, cancelable: true, shiftKey: true, keyCode: 36
        }))
    },

    input_kleft: (win, el) => {
        if (el.selectionStart != el.selectionEnd) {
            inputHandlers.input_erase(win, el)
            return
        }
        let val = el.value
        let cur = el.selectionStart
        let after = val.substring(cur)
        el.value = after
        el.selectionStart = el.selectionEnd = 0
    },

    input_kright: (win, el) => {
        if (el.selectionStart != el.selectionEnd) {
            inputHandlers.input_erase(win, el)
            return
        }
        let val = el.value
        let cur = el.selectionStart
        let before = val.substring(0, cur)
        el.value = before
        el.selectionStart = el.selectionEnd = cur
    },

    input_left: (win, el) => {
        el.dispatchEvent(new win.KeyboardEvent('keypress', {
            bubbles: true, cancelable: true, keyCode: 37
        }))
    },

    input_left_sel: (win, el) => {
        el.dispatchEvent(new win.KeyboardEvent('keypress', {
            bubbles: true, cancelable: true, shiftKey: true, keyCode: 37
        }))
    },

    input_right: (win, el) => {
        el.dispatchEvent(new win.KeyboardEvent('keypress', {
            bubbles: true, cancelable: true, keyCode: 39
        }))
    },

    input_right_sel: (win, el) => {
        el.dispatchEvent(new win.KeyboardEvent('keypress', {
            bubbles: true, cancelable: true, shiftKey: true, keyCode: 39
        }))
    },

    input_up: (win, el) => {
        el.dispatchEvent(new win.KeyboardEvent('keypress', {
            bubbles: true, cancelable: true, keyCode: 38
        }))
    },

    input_wdelete: (win, el) => {
        if (el.selectionStart != el.selectionEnd) {
            inputHandlers.input_erase(win, el)
            return
        }
        let val = el.value
        let cur = el.selectionStart
        let before = val.substring(0, cur)
        let after = val.substring(wmove(val, cur))
        el.value = before + after
        el.selectionStart = el.selectionEnd = cur
    },

    input_werase: (win, el) => {
        if (el.selectionStart != el.selectionEnd) {
            inputHandlers.input_erase(win, el)
            return
        }
        let val = el.value, curold = el.selectionStart
        let cur = wmove(val, curold, true)
        let before = val.substring(0, cur)
        let after = val.substring(curold)
        el.value = before + after
        el.selectionStart = el.selectionEnd = cur
    },

    input_wleft: (win, el) => {
        if (el.selectionStart != el.selectionEnd) {
            el.selectionStart = el.selectionEnd = el.selectionStart
            return
        }
        el.selectionStart = el.selectionEnd = wmove(el.value, el.selectionStart, true)
        //el.dispatchEvent(new win.KeyboardEvent('keypress', {
        //    bubbles: true, cancelable: true, ctrlKey: true, keyCode: 37
        //}))
    },

    input_wleft_sel: (win, el) => {
        if (el.selectionStart == el.selectionEnd) {
            el.selectionDirection = 'backward'
        }
        if (el.selectionDirection == 'backward') {
            el.selectionStart = wmove(el.value, el.selectionStart, true)
        } else {
            let idx = wmove(el.value, el.selectionEnd, true)
            if (idx < el.selectionStart) {
                el.selectionEnd = el.selectionStart
                el.selectionStart = idx
                el.selectionDirection = 'backward'
            } else {
                el.selectionEnd = idx
            }
        }
        //el.dispatchEvent(new win.KeyboardEvent('keypress', {
        //    bubbles: true, cancelable: true, ctrlKey: true, shiftKey: true, keyCode: 37
        //}))
    },

    input_wright: (win, el) => {
        if (el.selectionStart != el.selectionEnd) {
            el.selectionStart = el.selectionEnd = el.selectionEnd
            return
        }
        el.selectionStart = el.selectionEnd = wmove(el.value, el.selectionStart)
        //el.dispatchEvent(new win.KeyboardEvent('keypress', {
        //    bubbles: true, cancelable: true, ctrlKey: true, keyCode: 39
        //}))
    },

    input_wright_sel: (win, el) => {
        if (el.selectionStart == el.selectionEnd) {
            el.selectionDirection = 'forward'
        }
        if (el.selectionDirection == 'forward') {
            el.selectionEnd = wmove(el.value, el.selectionEnd)
        } else {
            let idx = wmove(el.value, el.selectionStart)
            if (idx > el.selectionEnd) {
                el.selectionStart = el.selectionEnd
                el.selectionEnd = idx
                el.selectionDirection = 'forward'
            } else {
                el.selectionStart = idx
            }
        }
        //el.dispatchEvent(new win.KeyboardEvent('keypress', {
        //    bubbles: true, cancelable: true, ctrlKey: true, shiftKey: true, keyCode: 39
        //}))
    },
}

function wmove(text, idx, backward = false) {
    let s, rs, i
    if (backward) {
        if (idx <= 0) {
            return 0
        }
        s = text.substring(0, idx)
        for (i = s.length, rs = ''; i > 0; i--) {
            rs += s[i - 1]
        }
        s = rs
    } else {
        if (idx >= text.length) {
            return text.length
        }
        s = text.substring(idx)
    }
    let m = s.match(/\s{2,}|\s?[^\s\w]+|\s?\w+/)
    if (m) {
        return idx + (backward ? -m[0].length : m[0].length)
    }
    return idx
}
