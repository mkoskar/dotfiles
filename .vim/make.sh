#!/bin/bash

set -e

maybe_cd() {
    cd "$1" >/dev/null 2>&1
}

from_git() {(
    local name=${1##*/}
    name=${name%%.git}
    echo "> $name : $1"

    if maybe_cd "$name"; then
        git fetch --all
        if [ "$(git rev-parse --abbrev-ref HEAD)" = 'local' ]; then
            echo '> _local_ branch - not merging!'
        else
            git merge --ff-only
        fi
    else
        git clone "$1"
        if [ -n "$2" ]; then
            cd "$name"
            git checkout -b local "$2"
        fi
    fi
)}

from_vba() {
    local vba=${1##*/} name=${vba%%.vba.gz}
    echo "> $name : $1"

    local http_code=$(curl -sL "$1" -o "$vba" -z "$vba" -w '%{http_code}')
    if [ "$http_code" = '200' ]; then
        rm -rf "$name"
        mkdir "$name"
        # Vimball doesn't work in Ex mode.
        # Let's keep it interactive to double check everything went OK.
        vim -N -X -i NONE -u NORC "$vba" -c "UseVimball $name" -c 'noremap q :quitall<CR>'
    fi
}

cd ~/.vim

mkdir -p .backup
mkdir -p .undo
mkdir -p .swap

mkdir -p autoload
ln -sf ../bundle/vim-pathogen/autoload/pathogen.vim autoload

(
    mkdir -p bundle; cd bundle
    from_git https://github.com/Lokaltog/vim-easymotion.git
    from_git https://github.com/SirVer/ultisnips.git
    from_git https://github.com/chriskempson/base16-vim.git
    from_git https://github.com/flazz/vim-colorschemes.git
    from_git https://github.com/jakar/vim-AnsiEsc.git
    from_git https://github.com/kana/vim-tabpagecd.git
    from_git https://github.com/kien/ctrlp.vim.git
    from_git https://github.com/majutsushi/tagbar.git
    from_git https://github.com/scrooloose/nerdcommenter.git
    from_git https://github.com/scrooloose/nerdtree.git
    from_git https://github.com/sjl/gundo.vim.git
    from_git https://github.com/tommcdo/vim-exchange.git
    from_git https://github.com/tpope/vim-fugitive.git
    from_git https://github.com/tpope/vim-pathogen.git
    from_git https://github.com/tpope/vim-repeat.git
    from_git https://github.com/tpope/vim-surround.git
    from_git https://github.com/vim-scripts/bufexplorer.zip.git
    from_git https://github.com/vim-scripts/minibufexpl.vim.git
    from_git https://github.com/vimoutliner/vimoutliner.git
)

(
    mkdir -p bundle_gpg; cd bundle_gpg
    from_git https://github.com/jamessan/vim-gnupg.git
)

#from_git https://github.com/AndrewRadev/tagfinder.vim.git
#from_git https://github.com/Raimondi/delimitMate
#from_git https://github.com/Shougo/unite.vim.git
#from_git https://github.com/Valloric/YouCompleteMe.git
#from_git https://github.com/airblade/vim-gitgutter.git
#from_git https://github.com/bling/vim-airline.git
#from_git https://github.com/justinmk/vim-gtfo
#from_git https://github.com/mattn/emmet-vim.git
#from_git https://github.com/mhinz/vim-signify.git
#from_git https://github.com/msanders/snipmate.vim.git
#from_git https://github.com/scrooloose/syntastic.git
#from_git https://github.com/tpope/vim-git.git
#from_git https://github.com/tpope/vim-markdown.git
#from_git https://github.com/tpope/vim-ragtag.git
#from_git https://github.com/tpope/vim-unimpaired.git
#from_git https://github.com/yuratomo/w3m.vim.git
