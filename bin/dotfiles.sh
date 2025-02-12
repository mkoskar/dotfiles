alias .f=dotfiles

dotfiles() {
    [ $# -gt 0 ] || set -- status -s
    command git --git-dir ~/.dotfiles/.git --work-tree ~/ "$@"
}

_dotfiles_init() {
    git clone -n https://github.com/mkoskar/dotfiles.git ~/.dotfiles
    dotfiles reset .
    dotfiles checkout-index -a
}
alias dotfiles-init=_dotfiles_init
