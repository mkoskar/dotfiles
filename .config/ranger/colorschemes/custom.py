# -*- coding: utf-8 -*-

from ranger.colorschemes.default import Default
from ranger.gui.color import *
import os


class Scheme(Default):

    def __init__(self):
        self.is_linux = os.environ['TERM'] == 'linux'

    def use(self, context):
        fg, bg, attr = Default.use(self, context)

        if context.in_browser:
            if context.selected and self.is_linux:
                attr &= ~bold

        elif context.in_titlebar:
            attr |= bold
            fg = white
            bg = default
            if context.hostname:
                attr &= ~bold
                fg = white
                if context.bad:
                    attr |= underline
            elif context.directory:
                fg = blue
            elif context.tab:
                attr &= ~bold
                if context.good:
                    attr |= bold
                    fg = white
            elif context.link:
                fg = cyan

        return fg, bg, attr
