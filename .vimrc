if !exists("$VIMDIR")
    let $VIMDIR = '~/.vim'
endif

"========== important
set nocompatible
set pastetoggle=<F6>
set timeout timeoutlen=1000 ttimeoutlen=10
let mapleader = ','
let maplocalleader = ';'

"========== moving around, searching and patterns
set whichwrap=b,s,<,>,[,]
" .       - current file directory
" (empty) - current working directory
" **      - recursive from current working directory
set path=.,,**,
set incsearch
set ignorecase
set smartcase

"========== tags
set tags=./tags,tags
if exists("$BASEDIR")
    exec 'set tags^='.$BASEDIR.'/tags'
endif

"set cscopetag
"set cscopequickfix=s-,d-,c-,t-,e-,i-

"nnoremap <silent> <C-_>s :lcs find s <C-R>=expand('<cword>')<CR><CR>
"nnoremap <silent> <C-_>g :lcs find g <C-R>=expand('<cword>')<CR><CR>
"nnoremap <silent> <C-_>d :lcs find d <C-R>=expand('<cword>')<CR><CR>
"nnoremap <silent> <C-_>c :lcs find c <C-R>=expand('<cword>')<CR><CR>
"nnoremap <silent> <C-_>t :lcs find t <C-R>=expand('<cword>')<CR><CR>
"nnoremap <silent> <C-_>e :lcs find e <C-R>=expand('<cword>')<CR><CR>
"nnoremap <silent> <C-_>f :lcs find f <C-R>=expand('<cfile>')<CR><CR>
"nnoremap <silent> <C-_>i :lcs find i ^<C-R>=expand('<cfile>')<CR>$<CR>

if exists("$BASEDIR")
    nnoremap <silent> <F12> :!$BASEDIR/tags.sh<CR>
endif

"========== displaying text
set scrolloff=10
set nowrap
set sidescroll=10
set sidescrolloff=10
set list
set listchars=eol:¬,tab:>-,trail:-,extends:>,precedes:<

nnoremap <silent> <Leader>l :set list!<CR>

"========== syntax, highlighting and spelling
set background=dark
set hlsearch
set colorcolumn=79,84
set spelllang=en_us

nnoremap <silent> <Space> :nohls<Bar>echo<CR>
nnoremap <silent> # :let @/ = "<C-R>=escape(escape(expand('<cWORD>'), '/\.*$^~["'), '''/\.*$^~["')<CR>"<Bar>set hls<CR>
nnoremap <silent> * :let @/ = "<C-R>=escape(escape(expand('<cword>'), '/\.*$^~["'), '''/\.*$^~["')<CR>"<Bar>set hls<CR>
nnoremap <silent> <Leader>sp :set spell!<CR>

"========== multiple windows
set laststatus=2
set statusline=
set statusline+=%2n
set statusline+=\ %<%f
set statusline+=%(\ [%M%W%R]%)
"set statusline+=%(\ %{fugitive#statusline()}%)
set statusline+=%(\ %y%)
set statusline+=%=
set statusline+=0x%-3B
set statusline+=\ %-14(%l,%c%V%)
set statusline+=\ %P\ 
set splitbelow
set splitright

" spatial navigation
nnoremap <C-J> <C-W>j
nnoremap <C-K> <C-W>k
nnoremap <C-H> <C-W>h
nnoremap <C-L> <C-W>l
nmap <BS> <C-H>

nnoremap <silent> c<C-J> :below sp<CR>
nnoremap <silent> c<C-K> :above sp<CR>
nnoremap <silent> c<C-H> :lefta vsp<CR>
nnoremap <silent> c<C-L> :rightb vsp<CR>
nmap c<BS> c<C-H>

nnoremap d<C-J> <C-W>j<C-W>c
nnoremap d<C-K> <C-W>k<C-W>c
nnoremap d<C-H> <C-W>h<C-W>c
nnoremap d<C-L> <C-W>l<C-W>c
nmap d<BS> d<C-H>

nnoremap <C-_> <C-W>_
nnoremap g<C-J> <C-W>j<C-W>_
nnoremap g<C-K> <C-W>k<C-W>_
nnoremap g<C-H> <C-W>h<C-W>_
nnoremap g<C-L> <C-W>l<C-W>_
nmap g<BS> g<C-H>

" faster resizing
nnoremap <silent> - :resize -5<CR>
nnoremap <silent> + :resize +5<CR>
nnoremap <silent> <C-W>, :vert resize -5<CR>
nnoremap <silent> <C-W>. :vert resize +5<CR>

"========== terminal
set nottybuiltin
set t_ut=

"========== using the mouse
set mouse=a

"========== GUI
set guicursor+=a:blinkon0
set guifont=Monaco\ 12
set guioptions+=c
set guioptions+=e
set guioptions-=m
set guioptions-=r
set guioptions-=L
set guioptions-=T

"========== messages and info
set shortmess=aoOtTWI
set showcmd
set showmode
set ruler
set novisualbell

"========== editing text
set textwidth=79
set backspace=indent,eol,start
set formatoptions=tcroqln21j
set tildeop
set showmatch

"========== tabs and indenting
set tabstop=4
set shiftwidth=4
set smarttab
set softtabstop=0
set expandtab
set autoindent
set smartindent
set copyindent
set preserveindent

"========== folding
set foldcolumn=0

nnoremap <Leader>ff :set fdm=manual<CR>
nnoremap <Leader>fi :set fdm=indent<CR>
nnoremap <Leader>fs :set fdm=syntax<CR>
nnoremap <Leader>fm :set fdm=marker<CR>

"========== reading and writing files
set modeline
set backup
exec 'set backupdir='.$VIMDIR.'/.backupdir//'
set autoread

"========== the swap file
exec 'set directory=~/.vim/.swapdir//,'.$TMPDIR.'//'
set swapfile

"========== command line editing
set history=500
set wildmenu
set wildmode=list:longest,full
set undofile
exec 'set undodir='.$VIMDIR.'/.undodir//'

"========== executing external commands
set shell=~/bin/bashx
set shellredir=>%s\ 2>&1

"========== running make and jumping to errors
set shellpipe=2>&1\|\ tee
set grepprg=grep\ -n\ --exclude-dir='.svn'\ --exclude-dir='.git'\ --exclude='*.swp'\ --exclude='*~'\ $*\ /dev/null

"========== multi-byte characters
set encoding=utf-8

"========== various
set virtualedit=block
set gdefault
exec 'set viminfo+=n'.$VIMDIR.'/.viminfo'

nnoremap cov :set <C-R>=(&virtualedit =~# 'all') ? 'virtualedit=block' : 'virtualedit=all'<CR><CR>

"========== other
syntax on
filetype plugin indent on

" make Man available
runtime ftplugin/man.vim

" let Y yank not entire line, but from cursor to the end (consistent with D, C)
nnoremap Y y$
" no Ex mode
nnoremap Q <nop>
" no Man lookup
nnoremap K <nop>
" no useless help messages
nnoremap <C-c> <silent> <C-c>

" operate on display lines not file lines
nnoremap j gj
nnoremap k gk
xnoremap j gj
xnoremap k gk

nmap <M-j> 5j
nmap <M-k> 5k
xmap <M-j> 5j
xmap <M-k> 5k

nnoremap <M-n> :tabnew<CR>
nnoremap <M-l> gt
nnoremap <M-h> gT
nnoremap <M-1> 1gt
nnoremap <M-2> 2gt
nnoremap <M-3> 3gt
nnoremap <M-4> 4gt
nnoremap <M-5> 5gt
nnoremap <M-6> 6gt
nnoremap <M-7> 7gt
nnoremap <M-8> 8gt
nnoremap <M-9> 9gt
nnoremap <silent> <M-0> :tablast<CR>

cnoremap %% <C-R>=expand('%:h').'/'<CR>
nmap <Leader>ew :e %%
nmap <Leader>es :sp %%
nmap <Leader>ev :vsp %%
nmap <Leader>et :tabe %%

nnoremap <silent> <C-X> :q<CR>
nnoremap <silent> <C-Q> :bd<CR>
nnoremap <silent> <Leader>rr :setl noreadonly modifiable<CR>

vnoremap <silent> <Leader>ss :sort<CR>
vnoremap <silent> <Leader>su :sort u<CR>
vnoremap <silent> <Leader>sd :sort!<CR>

" Working on diffs
nnoremap <silent> <Leader>du :diffup<CR>
nnoremap <silent> <Leader>d; :,diffget<CR>
nnoremap <silent> <Leader>d: :,diffput<CR>

" fast editing of the '.vimrc'
nnoremap <silent> <Leader>rc :vs ~/.vimrc<CR>

" XML prettify
vnoremap <silent> <Leader>px !tidy -config ~/.tidyrc -xml<CR>
nnoremap <silent> <Leader>px !!tidy -config ~/.tidyrc -xml<CR>

" Echos most recently caught exception (removing Vim 'class').
function! EchoException() abort
    redraw
    echohl ErrorMsg
    echo substitute(v:exception, '^Vim(.*):', '', 'g')
    echohl None
endfunction

" Echos last given error message.
function! EchoLastError() abort
    call EchoError(v:errmsg)
endfunction

" Echos given error message.
function! EchoError(errmsg) abort
    if a:errmsg != ''
        redraw
        echohl ErrorMsg | echomsg a:errmsg | echohl None
    endif
endfunction

" Performs exec of passed command in try/catch block catching all errors.
function! TryCatchAll(command) abort
    try
        exec a:command
    catch
        call EchoException()
    endtry
endfunction

" Write with elevated privileges.
function! s:Write(...) abort
    let file = '%'
    if a:0
        let file = a:1
    endif
    exec 'silent write !sudo tee >/dev/null '.file
    edit!
endfunction
command! -nargs=? W call s:Write(<f-args>)

" Toggles diff mode of current buffer.
function! s:DiffToggle() abort
   if &diff
        diffoff
    else
        diffthis
    endif
endfunction
command! DiffToggle call s:DiffToggle()
nnoremap <silent> <Leader>dd :DiffToggle<CR>

" Starts diff of current buffer with another file.
function! s:Diff2(file) abort
    if !filereadable(a:file)
        call EchoError("Diff2: can't read ".a:file)
        return
    endif
    let filetype = &ft
    diffthis
    exec 'vnew | r '.a:file.' | normal! 1Gdd'
    diffthis
    exec 'setl bt=nofile bh=wipe nobl noswf ft='.filetype
    exec 'silent file DIFF_'.a:file
endfunction
command! -nargs=1 -complete=file Diff2 call s:Diff2(<f-args>)
nnoremap <Leader>df :Diff2 <C-R>=expand('%')<CR>

" Closes current or last tab.
function! s:QuitTab(bang) abort
    try
        exec 'tabclose'.a:bang
    catch /E784/
        exec 'qall'.a:bang
    endtry
endfunction
command! -bang QuitTab call TryCatchAll("silent call s:QuitTab('<bang>')")
nnoremap <silent> QQ :QuitTab<CR>
nnoremap <silent> QA :QuitTab!<CR>

" A wrapper function to restore the cursor position, window position,
" and last search pattern after running a command.
function! Preserve(command) abort
    " save
    let last_search = @/
    let cursor_pos = getpos('.')
    normal H
    let window_pos = getpos('.')
    call setpos('.', cursor_pos)
    try
        exec a:command
    finally
        " restore
        call setpos('.', window_pos)
        normal zt
        call setpos('.', cursor_pos)
        let @/ = last_search
    endtry
endfunction
nnoremap <silent> <Leader>pp :call Preserve('%s/\s\+$//e')<CR>
vnoremap <silent> <Leader>pp :call Preserve("'<,'>s/\\s\\+$//e")<CR>
nnoremap <silent> <Leader>== :call Preserve('normal gg=G')<CR>

" Toggles translation of ASCII meta escape prefix encoding to 8 bit meta encoding.
let g:meta_enabled = 0
function! s:MetaToggle() abort
    let chars = '0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz'
    let i = 0
    let n = len(chars)
    while i < n
        let c = chars[i]
        if g:meta_enabled
            exec 'set <M-'.c.'>='
        else
            exec 'set <M-'.c.">=\e".c
        endif
        let i += 1
    endwhile
    let g:meta_enabled = !g:meta_enabled
    if !has('vim_starting')
        redraw
        echohl WarningMsg | echo 'Meta '.(g:meta_enabled ? 'ON' : 'OFF') | echohl None
    endif
endfunction
command! MetaToggle call s:MetaToggle()
nnoremap <silent> <Leader>mm :MetaToggle<CR>
silent MetaToggle

" Zoom / Restore window.
function! s:ZoomToggle() abort
    if exists('t:zoomed') && t:zoomed
        exec t:zoom_winrestcmd
        let t:zoomed = 0
    else
        let t:zoom_winrestcmd = winrestcmd()
        resize
        vertical resize
        let t:zoomed = 1
    endif
endfunction
command! ZoomToggle call s:ZoomToggle()
nnoremap <silent> <M-z> :ZoomToggle<CR>
nnoremap <silent> <M-m> :ZoomToggle<CR>

" Walks through list of colorschemes (q/C-C=quit, k=prev, default=next).
function! s:Themes() abort
    let themes = ['luciusblack', 'hybrid', 'bclear']
    let l = len(themes)
    if !exists('s:themes_last_index')
        let s:themes_last_index = 0
    endif
    let i = s:themes_last_index
    while 1
        let theme = themes[i]
        exec 'colorscheme '.theme
        redraw | echo theme
        let c = getchar()
        if c == 3 || c == 113
            break
        elseif c == 107
            let i = i > 0 ? i-1 : l-1
        else
            let i = i < l-1 ? i+1 : 0
        endif
    endwhile
    let s:themes_last_index = i
    redraw | echo
endfunction
command! Themes call s:Themes()
nnoremap <silent> <Leader>th :Themes<CR>

"=====================================================================
" 3rd party
"=====================================================================

function! s:MoveToPrevTab() abort
    if tabpagenr('$') == 1 && winnr('$') == 1
        return
    endif
    let l:tab_nr = tabpagenr('$')
    let l:cur_buf = bufnr('%')
    if tabpagenr() != 1
        close!
        if l:tab_nr == tabpagenr('$')
            tabprev
        endif
        vert topleft split
    else
        close!
        exe '0tabnew'
    endif
    exe 'b'.l:cur_buf
endfunc
command! MoveToPrevTab call s:MoveToPrevTab()
nnoremap <silent> <M-H> :MoveToPrevTab<CR>

function! s:MoveToNextTab() abort
    if tabpagenr('$') == 1 && winnr('$') == 1
        return
    endif
    let l:tab_nr = tabpagenr('$')
    let l:cur_buf = bufnr('%')
    if tabpagenr() < tab_nr
        close!
        if l:tab_nr == tabpagenr('$')
            tabnext
        endif
        vert topleft split
    else
        close!
        tabnew
    endif
    exe 'b'.l:cur_buf
endfunc
command! MoveToNextTab call s:MoveToNextTab()
nnoremap <silent> <M-L> :MoveToNextTab<CR>

"========== netrw
let g:netrw_alto = 1
let g:netrw_altv = 1
let g:netrw_banner = 1
let g:netrw_bufsettings = 'noma nomod nonu nowrap ro nolist cc=0 fdc=0'
let g:netrw_fastbrowse = 2
let g:netrw_list_cmd = 'sshm USEPORT HOSTNAME ls -Fa -I .'
let g:netrw_liststyle = 0
let g:netrw_preview = 1
let g:netrw_retmap = 1
let g:netrw_scp_cmd = 'scpm -q'
let g:netrw_sftp_cmd = 'sftpm'
let g:netrw_silent = 1
let g:netrw_special_syntax = 1
let g:netrw_winsize = 30

"========== pathogen
exec pathogen#infect()

set background=dark
if has('gui_running')
    colorscheme bclear
elseif &t_Co == 256
    colorscheme luciusblack
else
    colorscheme desert
endif

"========== nerdtree
"let g:NERDTreeQuitOnOpen = 1
let g:NERDTreeBookmarksFile = $VIMDIR.'/.NERDTreeBookmarks'
let g:NERDTreeCaseSensitiveSort = 1
let g:NERDTreeDirArrows = 1
let g:NERDTreeHijackNetrw = 0
let g:NERDTreeIgnore = []
let g:NERDTreeMapCWD = 'cD'
let g:NERDTreeShowBookmarks = 1
let g:NERDTreeShowHidden = 1
let g:NERDTreeWinSize = 36

noremap <silent> <F1> :NERDTreeFind<CR>
noremap <silent> <F2> :NERDTreeToggle<CR>
noremap <silent> <Leader>tf :NERDTreeFind<CR>
noremap <silent> <Leader>tt :NERDTreeToggle<CR>

"========== nerdcommenter
"let g:NERDSpaceDelims = 1

"========== ragtag
"let g:ragtag_globa_maps = 1

"========== bufexplorer
let g:bufExplorerFindActive = 0
let g:bufExplorerShowRelativePath = 1

"========== minibufexpl
"let g:miniBufExplMapCTabSwitchBufs = 1
"let g:miniBufExplMapCTabSwitchWindows = 1
let g:miniBufExplModSelTarget = 1
let g:miniBufExplUseSingleClick = 1
let g:miniBufExplorerMoreThanOne = 1000

noremap <silent> <Leader>bb :MiniBufExplorer<CR>

"========== buftabs
let g:buftabs_only_basename = 1

"========== taglist
"let g:Tlist_Close_On_Select = 1
let g:Tlist_Display_Tag_Scope = 0
let g:Tlist_Enable_Fold_Column = 0
let g:Tlist_GainFocus_On_ToggleOpen = 1
let g:Tlist_Show_One_File = 1
let g:Tlist_Use_Right_Window = 1
let g:Tlist_WinWidth = 35

nnoremap <silent> <F8> :TlistToggle<CR>

"========== tagbar
nnoremap <silent> <F8> :Tagbar<CR>

"========== tagfinder
"runtime plugin/tagfinder.vim
"DefineTagFinder FindClass c

"nnoremap <C-N> :FindClass 

"========== gundo
nnoremap <silent> <F4> :GundoToggle<CR>

"========== scratch
nnoremap <silent> <Leader>sc :Sscratch<CR>

"========== vim-airline
let g:airline#extensions#tabline#enabled = 1

"=====================================================================
" Autocommands
"=====================================================================

if has('autocmd')
augroup VIMRC
    autocmd!

    function! s:SpecialBufferSettings()
        setl nonu nowrap nolist cc=0 fdc=0
    endfunction

    function! s:ManBufferSettings()
        call s:SpecialBufferSettings()
        setl noma ts=8
    endfunction

    autocmd BufWinEnter -MiniBufExplorer- call s:SpecialBufferSettings()
    autocmd BufWinEnter -MiniBufExplorer- nnoremap <silent> <buffer> q :q<CR>
    autocmd BufWinEnter NERD_tree_* call s:SpecialBufferSettings()
    autocmd BufWinEnter NetrwMessage nnoremap <silent> <buffer> q :q<CR>
    autocmd BufWinEnter \[BufExplorer\] call s:SpecialBufferSettings()
    autocmd BufWinEnter __Tag_List__ call s:SpecialBufferSettings()
    autocmd BufWinEnter quickfix call s:SpecialBufferSettings()
    autocmd BufWritePost .Xresources !xrdb -load ~/.Xresources
    autocmd BufWritePost .vimrc source ~/.vimrc
    autocmd BufWritePre * let &backupext = '_'.substitute(expand('%:p:h'), '/', '%', 'g')
    autocmd CmdwinEnter * call s:SpecialBufferSettings()
    autocmd FileType make setl ts=4 sts=0 sw=4 noexpandtab
    autocmd FileType man call s:ManBufferSettings()
    autocmd FileType tar call s:SpecialBufferSettings()
    autocmd FileType zip call s:SpecialBufferSettings()
augroup END
endif
