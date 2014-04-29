#!/bin/bash

set -e

cd ~/.vim

mkdir -p .backupdir
mkdir -p .undodir

mkdir -p autoload
ln -sf ../bundle/vim-pathogen/autoload/pathogen.vim autoload

mkdir -p bundle
cd ./bundle

function maybe_cd() {
    cd "$1" >/dev/null 2>&1
}

function from_git() {
    local name
    name="${1##*/}"
    name="${name%%.git}"
    echo "> $name : $1"

    if maybe_cd "$name"; then
        git fetch
        git merge "${2:-master}"
    else
        git clone "$1"
        cd "$name"
        git checkout -b local "${2:-HEAD}"
    fi
    cd ..
}

function from_vba() {
    local vba name
    vba="${1##*/}"
    name="${vba%%.vba.gz}"
    echo "> $name : $1"

    http_code="$(curl -sL "$1" -o "$vba" -z "$vba" -w '%{http_code}')"
    if [ "$http_code" = '200' ]; then
        rm -rf "$name"
        mkdir "$name"
        # Vimball doesn't work in Ex mode.
        # Let's keep it interactive to double check everything went OK.
        vim -N -X -i NONE -u NORC "$vba" -c "UseVimball $name" -c 'noremap q :quitall<CR>'
    fi
}

function do_helptags() {
    vim -N -X -E -i NONE -u NORC -c "helptags $(readlink -f "$1") | quitall"
}

from_git https://github.com/AndrewRadev/tagfinder.vim.git
from_git https://github.com/jakar/vim-AnsiEsc.git
from_git https://github.com/mattn/emmet-vim.git
from_git https://github.com/msanders/snipmate.vim.git
from_git https://github.com/scrooloose/nerdcommenter.git
from_git https://github.com/scrooloose/nerdtree.git
from_git https://github.com/sjl/gundo.vim.git
from_git https://github.com/tpope/vim-fugitive.git
from_git https://github.com/tpope/vim-git.git
from_git https://github.com/tpope/vim-markdown.git
from_git https://github.com/tpope/vim-pathogen.git
from_git https://github.com/tpope/vim-ragtag.git
from_git https://github.com/tpope/vim-repeat.git
from_git https://github.com/tpope/vim-surround.git
from_git https://github.com/tpope/vim-unimpaired.git
from_git https://github.com/vim-scripts/Colour-Sampler-Pack.git
from_git https://github.com/vim-scripts/bufexplorer.zip.git
from_git https://github.com/vim-scripts/minibufexpl.vim.git

#for i in *; do
    #[ -d "$i" ] || continue
    #[ -d "$i/doc" ] || continue
    #do_helptags "$i/doc"
#done
