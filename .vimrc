let g:vimrc_done = 0

if exists('$BASEDIR')
    cd $BASEDIR
endif

if !exists('$VIMDIR')
    let $VIMDIR = '~/.vim'
endif

"========== important
set nocompatible
set pastetoggle=<F6>
set timeout timeoutlen=500 ttimeoutlen=10
let mapleader = ','

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
if exists('$BASEDIR')
    exec 'set tags^='.$BASEDIR.'/tags'
endif

"0 or s: Find this C symbol
"1 or g: Find this definition
"2 or d: Find functions called by this function
"3 or c: Find functions callling this function
"4 or it: Find this text string
"6 or e: Find this egrep pattern
"7 or f: Find this file
"8 or i: Find files #including this file

"set cscopetag
"set cscopequickfix=s-,d-,c-,t-,e-,i-

nnoremap <silent> <C-_>s :lcs find s <C-R>=expand('<cword>')<CR><CR>
nnoremap <silent> <C-_>g :lcs find g <C-R>=expand('<cword>')<CR><CR>
nnoremap <silent> <C-_>d :lcs find d <C-R>=expand('<cword>')<CR><CR>
nnoremap <silent> <C-_>c :lcs find c <C-R>=expand('<cword>')<CR><CR>
nnoremap <silent> <C-_>t :lcs find t <C-R>=expand('<cword>')<CR><CR>
nnoremap <silent> <C-_>e :lcs find e <C-R>=expand('<cword>')<CR><CR>
nnoremap <silent> <C-_>f :lcs find f <C-R>=expand('<cfile>')<CR><CR>
nnoremap <silent> <C-_>i :lcs find i ^<C-R>=expand('<cfile>')<CR>$<CR>

if exists('$BASEDIR')
    nnoremap <silent> <F12> :!$BASEDIR/tags.sh<CR>
endif

"========== displaying text
set scrolloff=10
set nowrap
set sidescroll=10
set sidescrolloff=10
set list
set listchars=eol:�,tab:>-,trail:-,extends:>,precedes:<

nnoremap <silent> <Leader>l :set list!<CR>

"========== syntax, highlighting and spelling
set background=dark
set hlsearch
set colorcolumn=79,84
set spelllang=en_us

nnoremap <silent> <Space> :nohls<Bar>echo<CR>
nnoremap <silent> # :let @/="<C-R>=escape(escape(expand('<cWORD>'), '/\.*$^~["'), '''/\.*$^~["')<CR>"<Bar>set hls<CR>
nnoremap <silent> * :let @/="<C-R>=escape(escape(expand('<cword>'), '/\.*$^~["'), '''/\.*$^~["')<CR>"<Bar>set hls<CR>

nnoremap <silent> <Leader>sp :set spell!<CR>

"========== multiple windows
set laststatus=2
set statusline=
set statusline+=%2n
set statusline+=\ %<%f
set statusline+=%(\ [%M%W%R]%)
set statusline+=%(\ %y%)
set statusline+=%=
set statusline+=0x%-3B
set statusline+=\ %-14(%l,%c%V%)
set statusline+=\ %P\ 
set splitbelow
set splitright

nnoremap <C-K> <C-W>k
nnoremap <C-J> <C-W>j
nnoremap <C-H> <C-W>h
nnoremap <BS> <C-W>h
nnoremap <C-L> <C-W>l

"nnoremap <C-W><M-k> 5<C-W>+
"nnoremap <C-W><M-j> 5<C-W>-
"nnoremap <C-W><M-h> 5<C-W>>
"nnoremap <C-W><M-l> 5<C-W><

"nnoremap <silent> <C-K> :<C-U>let @w=":resize +".v:count1."<C-V><CR>"<Bar>@w<CR>
"nnoremap <silent> <C-J> :<C-U>let @w=":resize -".v:count1."<C-V><CR>"<Bar>@w<CR>
"nnoremap <silent> <C-H> :<C-U>let @w=":vertical resize +".v:count1."<C-V><CR>"<Bar>@w<CR>
"nnoremap <silent> <C-L> :<C-U>let @w=":vertical resize -".v:count1."<C-V><CR>"<Bar>@w<CR>
"nnoremap <silent> <C-I> :@w<CR>
"noremap <silent> <C-W>. :@w<CR>

"========== terminal
set nottybuiltin
set t_ut=

"========== using the mouse
set mouse=a

"========== GUI
set guicursor+=a:blinkon0
set guifont=local_terminal_sized
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
set foldcolumn=1

nnoremap <Leader>ff :set fdm=manual<CR>
nnoremap <Leader>fi :set fdm=indent<CR>
nnoremap <Leader>fs :set fdm=syntax<CR>
nnoremap <Leader>fm :set fdm=marker<CR>

"========== reading and writing files
set modeline
set backup
exec 'set backupdir='.$VIMDIR.'/.backupdir'
set autoread

"========== the swap file
"set noswapfile

"========== command line editing
set history=500
set wildmenu
set wildmode=list:longest,full
"set wildoptions=tagfile
set undofile
exec 'set undodir='.$VIMDIR.'/.undodir'

"========== running make and jumping to errors
set grepprg=grep\ -n\ --exclude-dir='.svn'\ --exclude-dir='.git'\ --exclude='*.swp'\ --exclude='*~'\ $*\ /dev/null

"========== multi-byte characters
set encoding=utf-8

"========== various
set virtualedit=block
set gdefault
exec 'set viminfo+=n'.$VIMDIR.'/.viminfo'

"========== other
filetype plugin indent on
syntax enable

if &t_Co < 256
    colorscheme desert
else
    colorscheme luciusblack
endif

highlight Cursor guifg=white guibg=sienna2

" fast editing of the '.vimrc'
nnoremap <silent> <Leader>rc :vs ~/.vimrc<CR>

" write to system files
cmap w!! %!sudo tee > /dev/null %

" let Y yank not entire line, but from cursor to the end (consistent with D, C)
nnoremap Y y$
" no EX mode
nnoremap Q <nop>
" no MAN lookup
nnoremap K <nop>

nnoremap <up> <nop>
nnoremap <down> <nop>
nnoremap <left> <nop>
nnoremap <right> <nop>
inoremap <up> <nop>
inoremap <down> <nop>
inoremap <left> <nop>
inoremap <right> <nop>

" operate on display lines not file lines
nnoremap j gj
nnoremap k gk
xnoremap j gj
xnoremap k gk

nmap <M-j> 5j
nmap <M-j> 5j
nmap <M-k> 5k
xmap <M-j> 5j
xmap <M-k> 5k

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
nnoremap <Leader>ew :e %%
nnoremap <Leader>es :sp %%
nnoremap <Leader>ev :vsp %%
nnoremap <Leader>et :tabe %%

nnoremap <C-Q> :bd<CR>

"==============================================================================
" 3rd party
"==============================================================================

"========== pathogen
silent! call pathogen#infect()

"========== netrw
let g:netrw_list_cmd ='sshm USEPORT HOSTNAME ls -Fa -I .'
let g:netrw_scp_cmd ='scpm -q'
let g:netrw_sftp_cmd ='sftpm'
let g:netrw_bufsettings = 'noma nomod nonu nowrap ro nolist cc=0 fdc=0'
let g:netrw_alto = 1
let g:netrw_altv = 1
let g:netrw_fastbrowse = 0
let g:netrw_liststyle = 0
let g:netrw_retmap = 1
let g:netrw_silent = 1
"let g:netrw_special_syntax = 1

"========== nerdtree
let g:NERDTreeCaseSensitiveSort = 1
let g:NERDTreeHijackNetrw = 0
let g:NERDTreeIgnore = ['^\.svn$', '^\.git$', '\.swp$', '\~$']
let g:NERDTreeBookmarksFile = $VIMDIR.'/.NERDTreeBookmarks'
let g:NERDTreeShowBookmarks = 1
let g:NERDTreeShowHidden = 1
let g:NERDTreeWinSize = 36
let g:NERDTreeDirArrows = 1

noremap <silent> <F1> :NERDTreeFind<CR>
noremap <silent> <F2> :NERDTreeToggle<CR>

"========== nerdcommenter
"let g:NERDSpaceDelims = 1

"========== ragtag
"let g:ragtag_globa_maps = 1

"========== bufexplorer
let g:bufExplorerFindActive = 0
let g:bufExplorerShowRelativePath = 1

"========== minibufexpl
let g:miniBufExplorerMoreThanOne = 1000
"let g:miniBufExplMapCTabSwitchBufs = 1
"let g:miniBufExplMapCTabSwitchWindows = 1
let g:miniBufExplUseSingleClick = 1
let g:miniBufExplModSelTarget = 1

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

"========== tagfinder
runtime plugin/tagfinder.vim
DefineTagFinder FindClass c

nnoremap <C-N> :FindClass 

"========== gundo
nnoremap <silent> <F4> :GundoToggle<CR>

"========== scratch
nnoremap <silent> <Leader>ss :Sscratch<CR>

"========== other
nnoremap <silent> <Leader>mm :MetaToggle<CR>
command! MetaToggle call s:MetaToggle()
let g:meta_enabled = 0
function! s:MetaToggle()
    let chars = '0123456789abcdefghijklmnopqrstuvwxyz'
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
    if g:vimrc_done
        redraw
        echohl WarningMsg
        echo 'Meta '.(g:meta_enabled ? 'ON' : 'OFF')
    endif
endfunction
exec ':silent MetaToggle'

nnoremap <silent> QQ :QuitTab<CR>
command! QuitTab call s:QuitTab()
function! s:QuitTab()
    try
        tabclose
    catch /E784/ "Can't close last tab
        qall
    endtry
endfunction

nnoremap <silent> <Leader>pp :call Preserve(':%s/\s\+$//e')<CR>
function! Preserve(command)
    let _s=@/
    let pos = getpos('.')
    exec a:command
    let @/=_s
    call setpos('.', pos)
endfunction

command! -nargs=1 Diff2 call s:Diff2(expand('%'), <f-args>)
function! s:Diff2(file1, file2)
    silent exec 'tabnew | e '.a:file1.' | diffthis | vs | e '.a:file2.' | diffthis'
endfunction

" Find file in current directory and edit it.
command! -nargs=* Find :call Find(<f-args>)
function! Find(...)
    let path='.'
    if a:0==2
        let path=a:2
    endif
    let l:list=system("find ".path. " -name '".a:1."' | grep -v .svn ")
    let l:num=strlen(substitute(l:list, '[^\n]', '', 'g'))
    if l:num < 1
        echo "'".a:1."' not found"
        return
    endif
    if l:num == 1
        exec 'open ' . substitute(l:list, '\n', '', 'g')
    else
        let tmpfile = tempname()
        exec 'redir! > ' . tmpfile
        silent echon l:list
        redir END
        let old_efm = &efm
        set efm=%f
        if exists(':cgetfile')
            exec 'silent! cgetfile ' . tmpfile
        else
            exec 'silent! cfile ' . tmpfile
        endif
        let &efm = old_efm
        " Open the quickfix window below the current window
        botright copen
        call delete(tmpfile)
    endif
endfunction

"========== autocommands
if has('autocmd')
augroup VIMRC
    autocmd!
    " When '.vimrc' is written, reload it.
    autocmd BufWritePost .vimrc source ~/.vimrc

    " When '.Xresources' is written, reload it.
    autocmd BufWritePost .Xresources !xrdb -load ~/.Xresources

    autocmd FileType make setlocal ts=4 sts=0 sw=4 noexpandtab
    autocmd BufNewFile,BufRead *.rss setfiletype xml

    " Adjust settings of special buffers.
    function! s:SpecialBufferSettings()
        setlocal nonu nowrap nolist cc=0 fdc=0
    endfunction

    autocmd CmdwinEnter * call s:SpecialBufferSettings()
    autocmd BufWinEnter quickfix call s:SpecialBufferSettings()
    autocmd BufWinEnter NERD_tree_* call s:SpecialBufferSettings()
    autocmd BufWinEnter __Tag_List__ call s:SpecialBufferSettings()
    autocmd BufWinEnter \[BufExplorer\] call s:SpecialBufferSettings()
    autocmd BufWinEnter -MiniBufExplorer- call s:SpecialBufferSettings()
    autocmd FileType tar call s:SpecialBufferSettings()
    autocmd FileType zip call s:SpecialBufferSettings()
    autocmd BufWinEnter -MiniBufExplorer- nnoremap <silent> q :q<CR>
    autocmd BufWinEnter NetrwMessage nnoremap <silent> q :q<CR>
augroup END
endif

let g:vimrc_done = 1
