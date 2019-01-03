from percol.finder import FinderMultiQueryRegex

percol.view.prompt_replacees['F'] = \
    lambda self, **args: self.model.finder.get_name()
percol.view.prompt_replacees['C'] = \
    lambda self, **args: '' if self.model.finder.case_insensitive else 'cC '
percol.view.prompt_replacees['S'] = \
    lambda self, **args: '' if self.model.finder.split_query else 'S= '

percol.view.PROMPT = '> %q'
percol.view.RPROMPT = '%C%S(%F) [%i/%I] %n/%N'

percol.view.CANDIDATES_LINE_MARKED   = ('on_red',)
percol.view.CANDIDATES_LINE_QUERY    = ('yellow',)
percol.view.CANDIDATES_LINE_SELECTED = ('reverse',)

percol.import_keymap({
    'C-b' : lambda percol: percol.command.beginning_of_line(),
    'C-e' : lambda percol: percol.command.end_of_line(),
    'M-h' : lambda percol: percol.command.backward_char(),
    'M-l' : lambda percol: percol.command.forward_char(),
    'M-b' : lambda percol: percol.command.backward_word(),
    'M-f' : lambda percol: percol.command.forward_word(),
    'M-w' : lambda percol: percol.command.forward_word(),

    'C-u' : lambda percol: percol.command.clear_query(),
    'C-k' : lambda percol: percol.command.kill_end_of_line(),
    'C-w' : lambda percol: percol.command.delete_backward_word(),
    'M-d' : lambda percol: percol.command.delete_forward_word(),
    'M-x' : lambda percol: percol.command.delete_forward_char(),

    'C-j' : lambda percol: percol.finish(),
    'C-m' : lambda percol: percol.finish(),
    'C-g' : lambda percol: percol.cancel(),
    #'C-y' : lambda percol: percol.command.yank(),
    'M-m' : lambda percol: percol.command.mark_all(),
    'M-u' : lambda percol: percol.command.unmark_all(),

    'M-s' : lambda percol: percol.command.toggle_split_query(),
    'M-c' : lambda percol: percol.command.toggle_case_sensitive(),
    'M-r' : lambda percol: percol.command.toggle_finder(FinderMultiQueryRegex),

    'C-n' : lambda percol: percol.command.select_next(),
    'C-p' : lambda percol: percol.command.select_previous(),
    'M-n' : lambda percol: percol.command.select_next(),
    'M-p' : lambda percol: percol.command.select_previous(),
    'M-j' : lambda percol: percol.command.select_next_page(),
    'M-k' : lambda percol: percol.command.select_previous_page(),
    'M-J' : lambda percol: percol.command.select_bottom(),
    'M-K' : lambda percol: percol.command.select_top(),
})
