local lexers = vis.lexers

local fg = 7
local bg = 16

lexers.STYLE_DEFAULT = 'fore:' .. fg .. ',back:' .. bg

lexers.STYLE_COLOR_COLUMN = 'back:236'
lexers.STYLE_CURSOR = 'fore:1,back:15'
lexers.STYLE_CURSOR_LINE = 'back:236'
lexers.STYLE_CURSOR_PRIMARY = 'fore:16,back:214'
lexers.STYLE_EOF = 'fore:238'
lexers.STYLE_INFO = 'fore:228'
lexers.STYLE_LINENUMBER = 'fore:244,back:233'
lexers.STYLE_LINENUMBER_CURSOR = 'fore:231,back:233'
lexers.STYLE_SELECTION = 'back:235'
lexers.STYLE_SEPARATOR = 'fore:237,back:237'
lexers.STYLE_STATUS = 'fore:244,back:237,bold'
lexers.STYLE_STATUS_FOCUSED = 'fore:231,back:237,bold'

lexers.STYLE_COMMENT = 'fore:242'

lexers.STYLE_KEYWORD = 'fore:228'
lexers.STYLE_LABEL = 'fore:228'
lexers.STYLE_OPERATOR = 'fore:228'

lexers.STYLE_CLASS = 'fore:110'
lexers.STYLE_TYPE = 'fore:110'
lexers.STYLE_CONSTANT = 'fore:110'

lexers.STYLE_DEFINITION = 'fore:167'
lexers.STYLE_EMBEDDED = 'fore:167'
lexers.STYLE_PREPROCESSOR = 'fore:167'
lexers.STYLE_VARIABLE = 'fore:167'

lexers.STYLE_FUNCTION = 'fore:156'
lexers.STYLE_IDENTIFIER = 'fore:156'

lexers.STYLE_NOTHING = bg

lexers.STYLE_NUMBER = 'fore:174'
lexers.STYLE_REGEX = 'fore:174'
lexers.STYLE_STRING = 'fore:174'

lexers.STYLE_ERROR = 'fore:231,back:1'
lexers.STYLE_TAG = 'fore:231'
lexers.STYLE_WHITESPACE = 'fore:238'
