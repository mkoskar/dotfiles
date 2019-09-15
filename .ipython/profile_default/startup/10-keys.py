# -*- coding: utf-8 -*-

def _():
    from prompt_toolkit.filters.cli import \
        ViDigraphMode, \
        ViInsertMode, \
        ViInsertMultipleMode, \
        ViNavigationMode, \
        ViReplaceMode, \
        ViSelectionMode, \
        ViWaitingForTextObjectMode

    from prompt_toolkit.key_binding.bindings.named_commands import get_by_name
    from prompt_toolkit.keys import Key, Keys

    digraph_mode = ViDigraphMode()
    insert_mode = ViInsertMode()
    insert_multiple_mode = ViInsertMultipleMode()
    navigation_mode = ViNavigationMode()
    operator_given = ViWaitingForTextObjectMode()
    replace_mode = ViReplaceMode()
    selection_mode = ViSelectionMode()

    ip = get_ipython()
    registry = ip.pt_cli.application.key_bindings_registry

    registry.add_binding(Keys.ControlB, filter=insert_mode)(
            get_by_name('beginning-of-line'))
    registry.add_binding(Keys.ControlE, filter=insert_mode)(
            get_by_name('end-of-line'))
    #registry.add_binding(Keys.Escape, 'h', filter=insert_mode)(
    #        get_by_name('backward-char'))
    #registry.add_binding(Keys.Escape, 'l', filter=insert_mode)(
    #        get_by_name('forward-char'))
    #registry.add_binding(Keys.Escape, 'b', filter=insert_mode)(
    #        get_by_name('backward-word'))
    #registry.add_binding(Keys.Escape, 'f', filter=insert_mode)(
    #        get_by_name('forward-word'))
    #registry.add_binding(Keys.Escape, 'w', filter=insert_mode)(
    #        get_by_name('forward-word'))

    # <C-U> - unix-line-rubout
    registry.add_binding(Keys.ControlK, filter=insert_mode)(
            get_by_name('kill-line'))
    registry.add_binding(Keys.ControlW, filter=insert_mode)(
            get_by_name('backward-kill-word'))
    #registry.add_binding(Keys.Escape, 'd', filter=insert_mode)(
    #        get_by_name('kill-word'))
    #registry.add_binding(Keys.Escape, 'x', filter=insert_mode)(
    #        get_by_name('delete-char'))

    #@registry.add_binding(Keys.Escape, 'j', filter=insert_mode)
    #def _(event):
    #    event.current_buffer.cursor_down(count=event.arg)
    #@registry.add_binding(Keys.Escape, 'k', filter=insert_mode)
    #def _(event):
    #    event.current_buffer.cursor_up(count=event.arg)

    # <C-I> - menu-complete
    registry.add_binding(Keys.ControlO, filter=insert_mode)(
            get_by_name('menu-complete-backward'))
    #registry.add_binding(Keys.Escape, '#', filter=insert_mode)(
    #        get_by_name('insert-comment'))

    @registry.add_binding(Keys.ControlX, Keys.ControlE,
            filter=insert_mode|navigation_mode)
    def _(event):
        event.current_buffer.open_in_editor(event.cli)


#_()
del _
