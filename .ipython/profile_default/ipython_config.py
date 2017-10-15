# Configuration file for ipython.

from IPython.terminal.prompts import Token

c.InteractiveShell.autocall = 1
c.InteractiveShell.colors = 'Linux'
c.InteractiveShell.pdb = True
c.InteractiveShell.separate_in = ''
c.TerminalIPythonApp.display_banner = False
c.TerminalInteractiveShell.editing_mode = 'vi'
c.TerminalInteractiveShell.extra_open_editor_shortcuts = False
c.TerminalInteractiveShell.highlighting_style = 'rrt'

c.TerminalInteractiveShell.highlighting_style_overrides = {
    Token.OutPrompt: '#ansired',
    Token.Prompt: '#ansigreen',
    Token.ViMode: '#ansiyellow bold',
}
