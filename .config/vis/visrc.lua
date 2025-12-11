require('vis')

local modes = vis.modes

vis.events.subscribe(vis.events.INIT, function()

    vis:command('set autoindent')
    vis:command('set escdelay 10')
    vis:command('set shell bashx')
    vis:command('set theme custom')


    -- Moving Around, Searching and Patterns
    ----------------------------------------

    vis:command('map! normal <M-j> 5j')
    vis:command('map! normal <M-k> 5k')

    vis:command('map! normal <M-C-l> 10l')
    vis:command('map! normal <M-C-h> 10h')
    vis:command('map! normal <M-Backspace> <M-C-h>')

    vis:command('map! normal <C-e> 5<vis-window-slide-down>')
    vis:command('map! normal <C-y> 5<vis-window-slide-up>')


    -- Displaying Text
    ----------------------------------------

    vis:map(modes.NORMAL, ',L', function(keys)
        vis:command('set shownewlines!')
        vis:command('set showspaces!')
        vis:command('set showtabs!')
    end)


    -- Syntax, Highlighting and Spelling
    ----------------------------------------

    vis:command('map! normal cul ":set cursorline!<Enter>"')


    -- Multiple Windows
    ----------------------------------------

    vis:command('map! normal <C-j> <C-w>j')
    vis:command('map! normal <C-k> <C-w>k')
    vis:command('map! normal <C-l> <C-w>l')
    vis:command('map! normal <C-h> <C-w>h')
    vis:command('map! normal <Backspace> <C-h>')

    vis:command('map! normal s<C-j> <C-w>s')
    vis:command('map! normal s<C-k> <C-w>s<C-w>j')
    vis:command('map! normal s<C-l> <C-w>v')
    vis:command('map! normal s<C-h> <C-w>v<C-w>l')
    vis:command('map! normal s<Backspace> c<C-h>')

    vis:command('map! normal d<C-j> <C-w>j<C-w>c')
    vis:command('map! normal d<C-k> <C-w>k<C-w>c')
    vis:command('map! normal d<C-l> <C-w>l<C-w>c')
    vis:command('map! normal d<C-h> <C-w>h<C-w>c')
    vis:command('map! normal d<Backspace> d<C-h>')

    vis:command('map! normal ,q :q<Enter>')


    -- Editing Text
    ----------------------------------------

    vis:command('map! insert <C-b> <Home>')
    vis:command('map! insert <C-e> <End>')
    vis:command('map! insert <M-h> <Left>')
    vis:command('map! insert <M-l> <Right>')
    vis:command('map! insert <M-b> <S-Left>')
    vis:command('map! insert <M-f> <S-Right>')
    vis:command('map! insert <M-w> <S-Right>')

    -- <C-u>
    vis:command('map! insert <C-k> <vis-operator-delete>$')
    -- <C-w>
    vis:command('map! insert <M-d> <vis-operator-delete>w')
    vis:command('map! insert <M-x> <vis-delete-char-next>')

    --vis:command('map! insert <C-l>')
    vis:command('map! insert <M-j> <vis-motion-screenline-down>')
    vis:command('map! insert <M-k> <vis-motion-screenline-up>')
    vis:command('map! insert <C-n> <Down>')
    vis:command('map! insert <C-p> <Up>')


    -- Various
    ----------------------------------------

    vis:command('map! normal QQ :qa<Enter>')
    vis:command('map! normal QA :qa!<Enter>')
    vis:command('map! normal ,D :%d<Enter>')

    -- Open files faster
    vis:command('map! normal ,ew ":e "')
    vis:command('map! normal ,es ":sp "')
    vis:command('map! normal ,ev ":vsp "')
    vis:command('map! normal ,ee :e<Enter>')
    vis:command('map! normal ,E :e!<Enter>')

    -- Read / Filter faster
    vis:command('map! normal ,ri :<')
    vis:command('map! visual | :|')

    -- Sorting
    vis:command('map! visual ,ss :|sort<Enter>')
    vis:command('map! visual ,su ":|sort -u<Enter>"')
    vis:command('map! visual ,sr ":|sort -r<Enter>"')
    vis:command('map! visual ,sb :|sortb<Enter>')

    -- Strip / Squash lines
    vis:command('map! visual ,ps :|striplns<Enter>')
    vis:command('map! normal ,ps :%|striplns<Enter>')
    vis:command('map! visual ,pS :|squashlns<Enter>')
    vis:command('map! normal ,pS :%|squashlns<Enter>')

    -- URLs
    vis:command('map! visual ,uo ":>urls -o<Enter>"')
    vis:command('map! normal ,uo ":>urls -o<Enter>"')
    vis:command('map! normal ,uO ":%>urls -o<Enter>"')

    vis:command('map! visual ,uy ":>urls -y<Enter>"')
    vis:command('map! normal ,uy ":>urls -y<Enter>"')
    vis:command('map! normal ,uY ":%>urls -y<Enter>"')

end)

vis.events.subscribe(vis.events.WIN_OPEN, function(win)
    vis:command('set colorcolumn 80')
    vis:command('set expandtab')
    vis:command('set showeof')
    vis:command('set tabwidth 4')
end)
