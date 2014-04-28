#!/bin/bash

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

function git_update() {
    local name
    name="${1##*/}"
    name="${name%%.git}"
    echo "> $name : $1"
    (maybe_cd "./$name" && git pull || git clone "$1")
}

git_update https://github.com/AndrewRadev/tagfinder.vim.git
git_update https://github.com/jakar/vim-AnsiEsc.git
git_update https://github.com/mattn/emmet-vim.git
git_update https://github.com/msanders/snipmate.vim.git
git_update https://github.com/scrooloose/nerdcommenter.git
git_update https://github.com/scrooloose/nerdtree.git
git_update https://github.com/sjl/gundo.vim.git
git_update https://github.com/tpope/vim-fugitive.git
git_update https://github.com/tpope/vim-git.git
git_update https://github.com/tpope/vim-markdown.git
git_update https://github.com/tpope/vim-pathogen.git
git_update https://github.com/tpope/vim-ragtag.git
git_update https://github.com/tpope/vim-repeat.git
git_update https://github.com/tpope/vim-surround.git
git_update https://github.com/tpope/vim-unimpaired.git
git_update https://github.com/vim-scripts/Colour-Sampler-Pack.git
git_update https://github.com/vim-scripts/bufexplorer.zip.git
git_update https://github.com/vim-scripts/minibufexpl.vim.git
