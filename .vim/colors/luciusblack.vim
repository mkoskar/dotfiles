" vim: ft=vim fdm=marker

hi clear
if exists('syntax_on')
    syntax reset
endif
let colors_name = 'luciusblack'

set background=dark

hi Normal           guifg=#aeaeae           guibg=#050505
hi Normal           ctermfg=7               ctermbg=0

hi Comment          guifg=#909090                                   gui=NONE
hi Comment          ctermfg=245                                     cterm=NONE

hi Constant         guifg=#d78787                                   gui=NONE
hi Constant         ctermfg=174                                     cterm=NONE

hi Identifier       guifg=#afff87                                   gui=NONE
hi Identifier       ctermfg=156                                     cterm=NONE

hi Statement        guifg=#ffff87                                   gui=NONE
hi Statement        ctermfg=228                                     cterm=NONE

hi PreProc          guifg=#d75f5f                                   gui=NONE
hi PreProc          ctermfg=167                                     cterm=NONE

hi Type             guifg=#87afdf                                   gui=NONE
hi Type             ctermfg=110                                     cterm=NONE

hi Special          guifg=#eeeeee                                   gui=NONE
hi Special          ctermfg=255                                     cterm=NONE

" ========== Text Markup

" text that stands out, html links
hi Underlined       guifg=fg                                        gui=underline
hi Underlined       ctermfg=fg                                      cterm=underline

" any erroneous construct
hi Error            guifg=#d75f5f           guibg=#303030           gui=NONE
hi Error            ctermfg=167             ctermbg=236             cterm=NONE

" todo, fixme, note, xxx
hi Todo             guifg=#d7d787           guibg=NONE              gui=underline
hi Todo             ctermfg=186             ctermbg=NONE            cterm=underline

" match parenthesis, brackets
hi MatchParen       guifg=#00ff00           guibg=NONE              gui=bold
hi MatchParen       ctermfg=46              ctermbg=NONE            cterm=bold

" the '~' and '@' and showbreak, '>' double wide char doesn't fit on line
hi NonText          guifg=#4e4e4e
hi NonText          ctermfg=239

" meta and special keys used with map, unprintable characters
hi SpecialKey       guifg=#4e4e4e
hi SpecialKey       ctermfg=239

" titles for output from :set all, :autocmd, etc
hi Title            guifg=#5fafd7                                   gui=NONE
hi Title            ctermfg=74                                      cterm=NONE

" ========== Text Selection

" character under the cursor
hi Cursor           guifg=bg                guibg=#afd7ff
hi Cursor           ctermfg=bg              ctermbg=153

" like cursor, but used when in IME mode
hi CursorIM         guifg=bg                guibg=#87d7d7
hi CursorIM         ctermfg=bg              ctermbg=116

" cursor column
hi CursorColumn     guifg=NONE              guibg=#303030           gui=NONE
hi CursorColumn     ctermfg=NONE            ctermbg=236             cterm=NONE

" cursor line/row
hi CursorLine       guifg=NONE              guibg=#303030           gui=NONE
hi CursorLine       ctermfg=NONE            ctermbg=236             cterm=NONE

" visual mode selection
hi Visual           guifg=NONE              guibg=#262626
hi Visual           ctermfg=NONE            ctermbg=235

" visual mode selection when vim is not owning the selection (x11 only)
hi VisualNOS        guifg=fg                                        gui=underline
hi VisualNOS        ctermfg=fg                                      cterm=underline

" highlight incremental search text; also highlight text replaced with :s///c
hi IncSearch        guifg=#eeeeee                                   gui=reverse
hi IncSearch        ctermfg=255                                     cterm=reverse

" hlsearch (last search pattern), also used for quickfix
hi Search                                    guibg=#ffaf00          gui=NONE
hi Search                                    ctermbg=214            cterm=NONE

" ========== UI

" normal item in popup
hi Pmenu            guifg=#dadada           guibg=#121212           gui=NONE
hi Pmenu            ctermfg=253             ctermbg=233             cterm=NONE

" selected item in popup
hi PmenuSel         guifg=#d7d787           guibg=#3a3a3a           gui=NONE
hi PmenuSel         ctermfg=186             ctermbg=237             cterm=NONE

" scrollbar in popup
hi PMenuSbar                                guibg=#5f5f5f           gui=NONE
hi PMenuSbar                                ctermbg=59              cterm=NONE

" thumb of the scrollbar in the popup
hi PMenuThumb                               guibg=#878787           gui=NONE
hi PMenuThumb                               ctermbg=102             cterm=NONE

" status line for current window
hi StatusLine       guifg=#e4e4e4           guibg=#3a3a3a           gui=bold
hi StatusLine       ctermfg=254             ctermbg=237             cterm=bold

" status line for non-current windows
hi StatusLineNC     guifg=#808080           guibg=#3a3a3a           gui=NONE
hi StatusLineNC     ctermfg=244             ctermbg=237             cterm=NONE

" tab pages line, not active tab page label
hi TabLine          guifg=#808080           guibg=#303030           gui=NONE
hi TabLine          ctermfg=244             ctermbg=236             cterm=NONE

" tab pages line, where there are no labels
hi TabLineFill      guifg=#d7d7af           guibg=#303030           gui=NONE
hi TabLineFill      ctermfg=187             ctermbg=236             cterm=NONE

" tab pages line, active tab page label
hi TabLineSel       guifg=#e4e4e4           guibg=#303030           gui=bold
hi TabLineSel       ctermfg=254             ctermbg=236             cterm=bold

" column separating vertically split windows
hi VertSplit        guifg=#767676           guibg=#3a3a3a           gui=NONE
hi VertSplit        ctermfg=243             ctermbg=237             cterm=NONE

" line used for closed folds
hi Folded           guifg=#909090           guibg=NONE              gui=NONE
hi Folded           ctermfg=245             ctermbg=NONE            cterm=NONE

" column on side used to indicated open and closed folds
hi FoldColumn       guifg=#bcbcbc           guibg=#444444           gui=NONE
hi FoldColumn       ctermfg=250             ctermbg=238             cterm=NONE

" ========== Spelling

" word not recognized
hi SpellBad         guisp=#ee0000                                   gui=undercurl
hi SpellBad                                 ctermbg=196             cterm=undercurl

" word not capitalized
hi SpellCap         guisp=#eeee00                                   gui=undercurl
hi SpellCap                                 ctermbg=226             cterm=undercurl

" rare word
hi SpellRare        guisp=#ffa500                                   gui=undercurl
hi SpellRare                                ctermbg=214             cterm=undercurl

" wrong spelling for selected region
hi SpellLocal       guisp=#ffa500                                   gui=undercurl
hi SpellLocal                               ctermbg=214             cterm=undercurl

" ========== Diff

" added line
hi DiffAdd          guifg=fg                guibg=#005f00           gui=NONE
hi DiffAdd          ctermfg=fg              ctermbg=22              cterm=NONE

" changed line
hi DiffChange       guifg=fg                guibg=#5f0000           gui=NONE
hi DiffChange       ctermfg=fg              ctermbg=52              cterm=NONE

" deleted line
hi DiffDelete       guifg=fg                guibg=#5f5f00           gui=NONE
hi DiffDelete       ctermfg=fg              ctermbg=58              cterm=NONE

" changed text within line
hi DiffText         guifg=#ff5f5f           guibg=#5f0000           gui=bold
hi DiffText         ctermfg=203             ctermbg=52              cterm=bold

" ========== Misc

" directory names and other special names in listings
hi Directory        guifg=#afdfaf                                   gui=NONE
hi Directory        ctermfg=151                                     cterm=NONE

" error messages on the command line
hi ErrorMsg         guifg=#eeeeee           guibg=#9d2d2d           gui=NONE
hi ErrorMsg         ctermfg=255             ctermbg=1               cterm=NONE

" columns where signs are displayed (used in IDEs)
hi SignColumn       guifg=#afafaf           guibg=#121212           gui=NONE
hi SignColumn       ctermfg=145             ctermbg=233             cterm=NONE

" line numbers
hi LineNr           guifg=#909090           guibg=#121212
hi LineNr           ctermfg=245             ctermbg=233

" the 'more' prompt when output takes more than one line
hi MoreMsg          guifg=#00875f                                   gui=NONE
hi MoreMsg          ctermfg=29                                      cterm=NONE

" text showing what mode you are in
hi ModeMsg          guifg=#87d7ff           guibg=NONE              gui=NONE
hi ModeMsg          ctermfg=117             ctermbg=NONE            cterm=NONE

" the hit-enter prompt (show more output) and yes/no questions
hi Question         guifg=fg                                        gui=NONE
hi Question         ctermfg=fg                                      cterm=NONE

" warning messages
hi WarningMsg       guifg=#ffff87                                   gui=NONE
hi WarningMsg       ctermfg=228                                     cterm=NONE

" current match in the wildmenu completion
hi WildMenu         guifg=#000000           guibg=#d7d787           gui=bold,underline
hi WildMenu         ctermfg=16              ctermbg=186             cterm=bold

" color column highlighting
hi ColorColumn      guifg=NONE              guibg=#303030           gui=NONE
hi ColorColumn      ctermfg=NONE            ctermbg=236             cterm=NONE

" left blank, Hidden
hi Ignore           guifg=bg
hi Ignore           ctermfg=bg

" ========== Vimwiki Colors

hi link VimwikiHeader1 PreProcBold
hi link VimwikiHeader2 ConstantBold
hi link VimwikiHeader3 StatementBold
hi link VimwikiHeader4 IdentifierBold
hi link VimwikiHeader5 SpecialBold
hi link VimwikiHeader6 TypeBold
