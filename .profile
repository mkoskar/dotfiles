# ~/.profile

#echo '>>> ~/.profile' >&2

export PATH="$HOME/.local/bin:$PATH"
export PATH="$HOME/bin:$PATH"

[ -r "$HOME/.bashrc" ] && . "$HOME/.bashrc"
