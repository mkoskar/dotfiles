" vim: fdm=marker

set background=dark
hi clear
if exists('syntax_on')
    syntax reset
endif
let colors_name = 'luciusblack'

function! s:Highlight(link, group, ...) abort
    exec 'hi clear' a:group
    exec 'hi' (a:link ? 'link' : '') a:group join(a:000)
endfunction

command -nargs=* HiColor call s:Highlight(0, <f-args>)
command -nargs=* HiLink call s:Highlight(1, <f-args>)

if &t_Co >= 256 || has('gui_running')

    HiColor ColorColumn         ctermbg=236
    \                           guibg=#303030

    HiColor Conceal             ctermfg=238
    \                           guifg=#444444

    HiColor CurSearch           ctermfg=16      ctermbg=214
    \                           guifg=bg        guibg=#ffaf00

    HiColor Cursor              ctermfg=16      ctermbg=153
    \                           guifg=bg        guibg=#afd7ff

    HiColor CursorColumn        ctermbg=236
    \                           guibg=#303030

    HiColor CursorIM            ctermfg=16      ctermbg=116
    \                           guifg=bg        guibg=#87d7d7

    HiColor CursorLine          ctermbg=236
    \                           guibg=#303030

    HiColor CursorLineNr        ctermfg=231     ctermbg=233
    \                           guifg=#ffffff   guibg=#121212

    HiColor DiffAdd             ctermfg=15      ctermbg=241
    \                           guifg=#dddddd   guibg=#626262

    HiColor DiffChange          ctermfg=15      ctermbg=241
    \                           guifg=#dddddd   guibg=#626262

    HiColor DiffDelete          ctermfg=16      ctermbg=241
    \                           guifg=bg        guibg=#626262

    HiColor DiffText            ctermfg=231     ctermbg=1
    \                           guifg=#ffffff   guibg=#bf4343

    HiColor Directory           ctermfg=151
    \                           guifg=#afd7af

    HiColor EndOfBuffer         ctermfg=238
    \                           guifg=#444444

    HiColor ErrorMsg            ctermfg=231     ctermbg=1
    \                           guifg=#ffffff   guibg=#bf4343

    HiColor FoldColumn          ctermfg=240     ctermbg=233
    \                           guifg=#585858   guibg=#121212

    HiColor Folded              ctermfg=244     ctermbg=233
    \                           guifg=#808080   guibg=#121212

    HiColor IncSearch           ctermfg=16      ctermbg=231
    \                           guifg=bg        guibg=#ffffff

    HiColor LineNr              ctermfg=240     ctermbg=233
    \                           guifg=#585858   guibg=#121212

    HiColor MatchParen          ctermfg=46                      cterm=bold
    \                           guifg=#00ff00                   gui=bold

    HiColor ModeMsg             ctermfg=117
    \                           guifg=#87d7ff

    HiColor MoreMsg             ctermfg=228
    \                           guifg=#ffff87

    HiColor NonText             ctermfg=238
    \                           guifg=#444444

    HiColor Normal              ctermfg=7
    \                           guifg=#aeaeae   guibg=#000000

    HiColor Pmenu               ctermfg=fg      ctermbg=235
    \                           guifg=fg        guibg=#262626

    HiColor PmenuSbar           ctermbg=236
    \                           guibg=#303030

    HiColor PmenuSel            ctermfg=185     ctermbg=233
    \                           guifg=#d7d75f   guibg=#121212

    HiColor PmenuThumb          ctermbg=241
    \                           guibg=#626262

    HiColor Question            ctermfg=fg
    \                           guifg=fg

    HiColor QuickFixLine        ctermbg=236                     cterm=bold
    \                           guibg=#303030                   gui=bold

    HiColor Search              ctermfg=16      ctermbg=214
    \                           guifg=bg        guibg=#ffaf00

    HiColor SignColumn          ctermfg=145     ctermbg=233
    \                           guifg=#afafaf   guibg=#121212

    HiColor SpecialKey          ctermfg=238
    \                           guifg=#444444

    HiColor SpellBad            ctermfg=231     ctermbg=1
    \                           guifg=#ffffff   guibg=#bf4343

    HiColor SpellCap            ctermfg=16      ctermbg=228
    \                           guifg=bg        guibg=#ffff87

    HiColor SpellLocal          ctermfg=231     ctermbg=5
    \                           guifg=#ffffff   guibg=#bf43bf

    HiColor SpellRare           ctermfg=231     ctermbg=4
    \                           guifg=#ffffff   guibg=#4358bf

    HiColor StatusLine          ctermfg=231     ctermbg=237     cterm=bold
    \                           guifg=#ffffff   guibg=#3a3a3a   gui=bold

    HiColor StatusLineNC        ctermfg=244     ctermbg=237
    \                           guifg=#808080   guibg=#3a3a3a

    HiColor StatusLineTerm      ctermfg=231     ctermbg=237     cterm=bold
    \                           guifg=#ffffff   guibg=#3a3a3a   gui=bold

    HiColor StatusLineTermNC    ctermfg=244     ctermbg=237
    \                           guifg=#808080   guibg=#3a3a3a

    HiColor User1               ctermfg=231     ctermbg=1       cterm=bold
    \                           guifg=#ffffff   guibg=#bf4343   gui=bold

    HiColor TabLine             ctermfg=244     ctermbg=237
    \                           guifg=#808080   guibg=#3a3a3a

    HiColor TabLineFill         ctermfg=187     ctermbg=237
    \                           guifg=#d7d7af   guibg=#3a3a3a

    HiColor TabLineSel          ctermfg=231     ctermbg=237     cterm=bold
    \                           guifg=#ffffff   guibg=#3a3a3a   gui=bold

    HiColor Title               ctermfg=74
    \                           guifg=#5fafd7

    HiColor ToolbarButton       ctermfg=16      ctermbg=185     cterm=bold
    \                           guifg=bg        guibg=#d7d75f   gui=bold

    HiColor ToolbarLine         ctermbg=236
    \                           guibg=#303030

    HiColor VertSplit           ctermfg=243     ctermbg=237
    \                           guifg=#767676   guibg=#3a3a3a

    HiColor Visual              ctermbg=235
    \                           guibg=#262626

    HiColor VisualNOS           ctermfg=fg                      cterm=underline
    \                           guifg=fg                        gui=underline

    HiColor WarningMsg          ctermfg=228
    \                           guifg=#ffff87

    HiColor Whitespace          ctermfg=238
    \                           guifg=#444444

    HiColor WildMenu            ctermfg=16      ctermbg=185     cterm=bold
    \                           guifg=bg        guibg=#d7d75f   gui=bold

    HiLink WinSeparator         VertSplit

    " CursorLineFold
    " CursorLineSign
    " FloatBorder
    " FloatFooter
    " FloatTitle
    " LineNrAbove
    " LineNrBelow
    " MessageWindow
    " MsgArea
    " MsgSeparator
    " NormalFloat
    " NormalNC
    " PmenuExtra
    " PmenuExtraSel
    " PmenuKind
    " PmenuKindSel
    " PmenuMatch
    " PmenuMatchSel
    " PopupNotification
    " SnippetTabstop
    " Substitute
    " TermCursor
    " TermCursorNC
    " Terminal
    " WinBar
    " WinBarNC
    " lCursor

    " ----------------------------------------

    HiColor Comment             ctermfg=242
    \                           guifg=#6c6c6c

    HiColor Constant            ctermfg=174
    \                           guifg=#d78787

    HiColor Error               ctermfg=167     ctermbg=234
    \                           guifg=#d75f5f   guibg=#1c1c1c

    HiColor Identifier          ctermfg=156
    \                           guifg=#afff87

    HiColor Ignore              ctermfg=16
    \                           guifg=bg

    HiColor PreProc             ctermfg=167
    \                           guifg=#d75f5f

    HiColor Special             ctermfg=231
    \                           guifg=#ffffff

    HiColor Statement           ctermfg=228
    \                           guifg=#ffff87

    HiColor Todo                ctermfg=185     ctermbg=234
    \                           guifg=#d7d75f   guibg=#1c1c1c

    HiColor Type                ctermfg=110
    \                           guifg=#87afd7

    HiColor Underlined          ctermfg=fg                      cterm=underline
    \                           guifg=fg                        gui=underline

    " Added
    " Changed
    " Removed

    " ----------------------------------------

    HiLink Boolean              Constant
    HiLink Character            Constant
    HiLink Conditional          Statement
    HiLink Debug                Special
    HiLink Define               PreProc
    HiLink Delimiter            Special
    HiLink Exception            Statement
    HiLink Float                Number
    HiLink Function             Identifier
    HiLink Include              PreProc
    HiLink Keyword              Statement
    HiLink Label                Statement
    HiLink Macro                PreProc
    HiLink Number               Constant
    HiLink Operator             Statement
    HiLink PreCondit            PreProc
    HiLink Repeat               Statement
    HiLink SpecialChar          Special
    HiLink SpecialComment       Special
    HiLink StorageClass         Type
    HiLink String               Constant
    HiLink Structure            Type
    HiLink Tag                  Special
    HiLink Typedef              Type

    HiLink HelpCommand          Statement
    HiLink HelpExample          Statement

    " ----------------------------------------

    HiColor LspReferenceRead ctermfg=16 ctermbg=114 cterm=underline,italic
    HiColor LspReferenceText ctermfg=16 ctermbg=80 cterm=underline,italic
    HiColor LspReferenceWrite ctermfg=16 ctermbg=254 cterm=underline,italic

    HiColor LspDiagnosticsDefaultError ctermfg=167 ctermbg=233 cterm=italic
    HiColor LspDiagnosticsDefaultInformation ctermfg=117 ctermbg=233 cterm=italic
    HiColor LspDiagnosticsDefaultWarning ctermfg=228 ctermbg=233 cterm=italic

    HiColor LspDiagnosticsFloatingError ctermfg=167 cterm=italic
    HiColor LspDiagnosticsFloatingInformation ctermfg=117 cterm=italic
    HiColor LspDiagnosticsFloatingWarning ctermfg=228 cterm=italic

    HiColor LspDiagnosticsUnderlineError ctermfg=16 ctermbg=167 cterm=italic
    HiColor LspDiagnosticsUnderlineInformation ctermfg=16 ctermbg=117 cterm=italic
    HiColor LspDiagnosticsUnderlineWarning ctermfg=16 ctermbg=228 cterm=italic

    HiColor LspCodeLens ctermfg=167 cterm=italic
    HiColor LspCodeLensSeparator ctermfg=167 cterm=italic

    HiColor LspSignatureActiveParameter ctermfg=167 cterm=italic

    HiLink LspDiagnosticsDefaultHint        LspDiagnosticsDefaultInformation
    HiLink LspDiagnosticsFloatingHint       LspDiagnosticsFloatingInformation
    HiLink LspDiagnosticsSignHint           LspDiagnosticsSignInformation
    HiLink LspDiagnosticsUnderlineHint      LspDiagnosticsUnderlineInformation
    HiLink LspDiagnosticsVirtualTextHint    LspDiagnosticsVirtualTextInformation

    " ----------------------------------------

    HiColor TagbarHighlight     cterm=reverse
    HiLink  TagbarHelp          String

    HiLink EasyMotionIncCursor          Cursor
    HiLink EasyMotionIncSearch          IncSearch
    HiLink EasyMotionMoveHL             IncSearch
    HiLink EasyMotionShade              Comment
    HiLink EasyMotionTarget2First       EasyMotionTarget
    HiLink EasyMotionTarget2Second      EasyMotionTarget

    HiLink asciidocQuotedEmphasized     PreProc

    HiLink bufExplorerActBuf    Directory
    HiLink bufExplorerAltBuf    Type
    HiLink bufExplorerCurBuf    Statement
    HiLink bufExplorerHelp      String
    HiLink bufExplorerHidBuf    Normal
    HiLink bufExplorerInactBuf  Normal
    HiLink bufExplorerMapping   Identifier
    HiLink bufExplorerTitle     PreProc

    HiLink diffBDiffer          Statement
    HiLink diffCommon           Statement
    HiLink diffDiffer           Statement
    HiLink diffIdentical        Statement
    HiLink diffIndexLine        Type
    HiLink diffIsA              Statement
    HiLink diffNoEOL            Statement
    HiLink diffOnly             Statement
    HiLink diffRemoved          Constant

    HiLink htmlBold             Normal
    HiLink htmlEndTag           htmlTagName
    HiLink htmlItalic           Normal
    HiLink htmlLink             Function
    HiLink htmlSpecialTagName   htmlTagName
    HiLink htmlTag              htmlTagName

    HiLink markdownItalic       PreProc

    HiLink xmlEndTag            Statement
    HiLink xmlTag               Statement
    HiLink xmlTagName           Statement

endif

delcommand HiColor
delcommand HiLink
