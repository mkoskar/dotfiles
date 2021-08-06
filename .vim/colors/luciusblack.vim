" vim: fdm=marker

set background=dark
hi clear
if exists('syntax_on')
    syntax reset
endif
let colors_name = 'luciusblack'

if &t_Co >= 256 || has('gui_running')
    hi Normal           ctermfg=7           ctermbg=NONE        cterm=NONE
    hi Normal           guifg=#aeaeae       guibg=#000000       gui=NONE

    hi ColorColumn      ctermfg=NONE        ctermbg=236         cterm=NONE
    hi ColorColumn      guifg=NONE          guibg=#303030       gui=NONE
    hi Comment          ctermfg=242         ctermbg=NONE        cterm=NONE
    hi Comment          guifg=#6c6c6c       guibg=NONE          gui=NONE
    hi Conceal          ctermfg=238         ctermbg=NONE        cterm=NONE
    hi Conceal          guifg=#444444       guibg=NONE          gui=NONE
    hi Constant         ctermfg=174         ctermbg=NONE        cterm=NONE
    hi Constant         guifg=#d78787       guibg=NONE          gui=NONE
    hi Cursor           ctermfg=16          ctermbg=153         cterm=NONE
    hi Cursor           guifg=bg            guibg=#afd7ff       gui=NONE
    hi CursorColumn     ctermfg=NONE        ctermbg=236         cterm=NONE
    hi CursorColumn     guifg=NONE          guibg=#303030       gui=NONE
    hi CursorIM         ctermfg=16          ctermbg=116         cterm=NONE
    hi CursorIM         guifg=bg            guibg=#87d7d7       gui=NONE
    hi CursorLine       ctermfg=NONE        ctermbg=236         cterm=NONE
    hi CursorLine       guifg=NONE          guibg=#303030       gui=NONE
    hi CursorLineNr     ctermfg=231         ctermbg=233         cterm=NONE
    hi CursorLineNr     guifg=#ffffff       guibg=#121212       gui=NONE
    hi DiffAdd          ctermfg=15          ctermbg=241         cterm=NONE
    hi DiffAdd          guifg=#dddddd       guibg=#626262       gui=NONE
    hi DiffChange       ctermfg=15          ctermbg=241         cterm=NONE
    hi DiffChange       guifg=#dddddd       guibg=#626262       gui=NONE
    hi DiffDelete       ctermfg=16          ctermbg=241         cterm=NONE
    hi DiffDelete       guifg=bg            guibg=#626262       gui=NONE
    hi DiffText         ctermfg=231         ctermbg=1           cterm=NONE
    hi DiffText         guifg=#ffffff       guibg=#9d2d2d       gui=NONE
    hi Directory        ctermfg=151         ctermbg=NONE        cterm=NONE
    hi Directory        guifg=#afd7af       guibg=NONE          gui=NONE
    hi EndOfBuffer      ctermfg=238         ctermbg=NONE        cterm=NONE
    hi EndOfBuffer      guifg=#444444       guibg=NONE          gui=NONE
    hi Error            ctermfg=167         ctermbg=234         cterm=NONE
    hi Error            guifg=#d75f5f       guibg=#1c1c1c       gui=NONE
    hi ErrorMsg         ctermfg=231         ctermbg=1           cterm=NONE
    hi ErrorMsg         guifg=#ffffff       guibg=#9d2d2d       gui=NONE
    hi FoldColumn       ctermfg=250         ctermbg=233         cterm=NONE
    hi FoldColumn       guifg=#bcbcbc       guibg=#121212       gui=NONE
    hi Folded           ctermfg=244         ctermbg=233         cterm=NONE
    hi Folded           guifg=#808080       guibg=#121212       gui=NONE
    hi Identifier       ctermfg=156         ctermbg=NONE        cterm=NONE
    hi Identifier       guifg=#afff87       guibg=NONE          gui=NONE
    hi Ignore           ctermfg=16          ctermbg=NONE        cterm=NONE
    hi Ignore           guifg=bg            guibg=NONE          gui=NONE
    hi IncSearch        ctermfg=16          ctermbg=231         cterm=NONE
    hi IncSearch        guifg=bg            guibg=#ffffff       gui=NONE
    hi LineNr           ctermfg=244         ctermbg=233         cterm=NONE
    hi LineNr           guifg=#808080       guibg=#121212       gui=NONE
    hi MatchParen       ctermfg=46          ctermbg=NONE        cterm=bold
    hi MatchParen       guifg=#00ff00       guibg=NONE          gui=bold
    hi ModeMsg          ctermfg=117         ctermbg=NONE        cterm=NONE
    hi ModeMsg          guifg=#87d7ff       guibg=NONE          gui=NONE
    hi MoreMsg          ctermfg=228         ctermbg=NONE        cterm=NONE
    hi MoreMsg          guifg=#ffff87       guibg=NONE          gui=NONE
    hi NonText          ctermfg=238         ctermbg=NONE        cterm=NONE
    hi NonText          guifg=#444444       guibg=NONE          gui=NONE
    hi Pmenu            ctermfg=fg          ctermbg=235         cterm=NONE
    hi Pmenu            guifg=fg            guibg=#262626       gui=NONE
    hi PmenuSbar        ctermfg=NONE        ctermbg=236         cterm=NONE
    hi PmenuSbar        guifg=NONE          guibg=#303030       gui=NONE
    hi PmenuSel         ctermfg=185         ctermbg=233         cterm=NONE
    hi PmenuSel         guifg=#d7d75f       guibg=#121212       gui=NONE
    hi PmenuThumb       ctermfg=NONE        ctermbg=241         cterm=NONE
    hi PmenuThumb       guifg=NONE          guibg=#626262       gui=NONE
    hi PreProc          ctermfg=167         ctermbg=NONE        cterm=NONE
    hi PreProc          guifg=#d75f5f       guibg=NONE          gui=NONE
    hi Question         ctermfg=fg          ctermbg=NONE        cterm=NONE
    hi Question         guifg=fg            guibg=NONE          gui=NONE
    hi QuickFixLine     ctermfg=NONE        ctermbg=236         cterm=bold
    hi QuickFixLine     guifg=NONE          guibg=#303030       gui=bold
    hi Search           ctermfg=16          ctermbg=214         cterm=NONE
    hi Search           guifg=bg            guibg=#ffaf00       gui=NONE
    hi SignColumn       ctermfg=145         ctermbg=233         cterm=NONE
    hi SignColumn       guifg=#afafaf       guibg=#121212       gui=NONE
    hi Special          ctermfg=231         ctermbg=NONE        cterm=NONE
    hi Special          guifg=#ffffff       guibg=NONE          gui=NONE
    hi SpecialKey       ctermfg=238         ctermbg=NONE        cterm=NONE
    hi SpecialKey       guifg=#444444       guibg=NONE          gui=NONE
    hi SpellBad         ctermfg=231         ctermbg=1           cterm=NONE
    hi SpellBad         guifg=#ffffff       guibg=#9d2d2d       gui=NONE
    hi SpellCap         ctermfg=16          ctermbg=228         cterm=NONE
    hi SpellCap         guifg=bg            guibg=#ffff87       gui=NONE
    hi SpellLocal       ctermfg=231         ctermbg=5           cterm=NONE
    hi SpellLocal       guifg=#ffffff       guibg=#a43fa4       gui=NONE
    hi SpellRare        ctermfg=231         ctermbg=4           cterm=NONE
    hi SpellRare        guifg=#ffffff       guibg=#384580       gui=NONE
    hi Statement        ctermfg=228         ctermbg=NONE        cterm=NONE
    hi Statement        guifg=#ffff87       guibg=NONE          gui=NONE
    hi StatusLine       ctermfg=231         ctermbg=237         cterm=bold
    hi StatusLine       guifg=#ffffff       guibg=#3a3a3a       gui=bold
    hi StatusLineNC     ctermfg=244         ctermbg=237         cterm=NONE
    hi StatusLineNC     guifg=#808080       guibg=#3a3a3a       gui=NONE
    hi StatusLineTerm   ctermfg=231         ctermbg=237         cterm=bold
    hi StatusLineTerm   guifg=#ffffff       guibg=#3a3a3a       gui=bold
    hi StatusLineTermNC ctermfg=244         ctermbg=237         cterm=NONE
    hi StatusLineTermNC guifg=#808080       guibg=#3a3a3a       gui=NONE
    hi TabLine          ctermfg=244         ctermbg=237         cterm=NONE
    hi TabLine          guifg=#808080       guibg=#3a3a3a       gui=NONE
    hi TabLineFill      ctermfg=187         ctermbg=237         cterm=NONE
    hi TabLineFill      guifg=#d7d7af       guibg=#3a3a3a       gui=NONE
    hi TabLineSel       ctermfg=231         ctermbg=237         cterm=bold
    hi TabLineSel       guifg=#ffffff       guibg=#3a3a3a       gui=bold
    hi Title            ctermfg=74          ctermbg=NONE        cterm=NONE
    hi Title            guifg=#5fafd7       guibg=NONE          gui=NONE
    hi Todo             ctermfg=185         ctermbg=234         cterm=NONE
    hi Todo             guifg=#d7d75f       guibg=#1c1c1c       gui=NONE
    hi ToolbarButton    ctermfg=16          ctermbg=185         cterm=bold
    hi ToolbarButton    guifg=bg            guibg=#d7d75f       gui=bold
    hi ToolbarLine      ctermfg=NONE        ctermbg=236         cterm=NONE
    hi ToolbarLine      guifg=NONE          guibg=#303030       gui=NONE
    hi Type             ctermfg=110         ctermbg=NONE        cterm=NONE
    hi Type             guifg=#87afd7       guibg=NONE          gui=NONE
    hi Underlined       ctermfg=fg          ctermbg=NONE        cterm=underline
    hi Underlined       guifg=fg            guibg=NONE          gui=underline
    hi VertSplit        ctermfg=243         ctermbg=237         cterm=NONE
    hi VertSplit        guifg=#767676       guibg=#3a3a3a       gui=NONE
    hi Visual           ctermfg=NONE        ctermbg=235         cterm=NONE
    hi Visual           guifg=NONE          guibg=#262626       gui=NONE
    hi VisualNOS        ctermfg=fg          ctermbg=NONE        cterm=underline
    hi VisualNOS        guifg=fg            guibg=NONE          gui=underline
    hi WarningMsg       ctermfg=228         ctermbg=NONE        cterm=NONE
    hi WarningMsg       guifg=#ffff87       guibg=NONE          gui=NONE
    hi WildMenu         ctermfg=16          ctermbg=185         cterm=bold
    hi WildMenu         guifg=bg            guibg=#d7d75f       gui=bold

    hi LspReferenceText ctermfg=16 ctermbg=80 cterm=underline,italic
    hi LspReferenceRead ctermfg=16 ctermbg=114 cterm=underline,italic
    hi LspReferenceWrite ctermfg=16 ctermbg=254 cterm=underline,italic

    hi LspDiagnosticsDefaultError ctermfg=167 ctermbg=233 cterm=italic
    hi LspDiagnosticsDefaultWarning ctermfg=228 ctermbg=233 cterm=italic
    hi LspDiagnosticsDefaultInformation ctermfg=117 ctermbg=233 cterm=italic
    hi link LspDiagnosticsDefaultHint LspDiagnosticsDefaultInformation

    hi LspDiagnosticsUnderlineError ctermfg=16 ctermbg=167 cterm=italic
    hi LspDiagnosticsUnderlineWarning ctermfg=16 ctermbg=228 cterm=italic
    hi LspDiagnosticsUnderlineInformation ctermfg=16 ctermbg=117 cterm=italic
    hi link LspDiagnosticsUnderlineHint LspDiagnosticsUnderlineInformation

    hi LspDiagnosticsFloatingError ctermfg=167 cterm=italic
    hi LspDiagnosticsFloatingWarning ctermfg=228 cterm=italic
    hi LspDiagnosticsFloatingInformation ctermfg=117 cterm=italic
    hi link LspDiagnosticsFloatingHint LspDiagnosticsFloatingInformation

    hi LspCodeLens ctermfg=167 cterm=italic
    hi LspCodeLensSeparator ctermfg=167 cterm=italic

    hi LspSignatureActiveParameter ctermfg=167 cterm=italic
endif
