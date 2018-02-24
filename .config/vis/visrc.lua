require('vis')

local modes = vis.modes

vis.events.subscribe(vis.events.INIT, function()

    vis:command('set autoindent')
    vis:command('set escdelay 10')
    vis:command('set expandtab')
    vis:command('set shell bashx')
    vis:command('set tabwidth 4')
    vis:command('set theme default-256')

    vis:command('map! normal <M-j> 5j')
    vis:command('map! normal <M-k> 5k')

    vis:command('map! normal <M-C-l> 10l')
    vis:command('map! normal <M-C-h> 10h')
    vis:command('map! normal <M-Backspace> <M-C-h>')

    vis:command('map! normal <C-e> 5<vis-window-slide-down>')
    vis:command('map! normal <C-y> 5<vis-window-slide-up>')

    vis:map(modes.NORMAL, ',L', function(keys)
        vis:command('set show-newlines!')
        vis:command('set show-spaces!')
    end, 'Toggle show-newlines and show-spaces')

    vis:command('map! normal ,cul ":set cursorline!<Enter>"')

    vis:command('map! normal <C-j> <C-w>j')
    vis:command('map! normal <C-k> <C-w>k')
    vis:command('map! normal <C-l> <C-w>l')
    vis:command('map! normal <C-h> <C-w>h')
    vis:command('map! normal <Backspace> <C-h>')

    vis:command('map! normal ,q :q<Enter>')
    vis:command('map! normal ,D :%d<Enter>')

    vis:command('map! normal ,ew ":e "')
    vis:command('map! normal ,es ":sp "')
    vis:command('map! normal ,ev ":vsp "')
    vis:command('map! normal ,ee :e<Enter>')
    vis:command('map! normal ,E :e!<Enter>')

end)

vis.events.subscribe(vis.events.WIN_OPEN, function(win)

    vis:command('set colorcolumn 80')
    vis:command('set show-eof')
    vis:command('set show-tabs')

end)
