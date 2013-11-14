# ~/.profile

#echo '>>> ~/.profile' >&2

export PATH="$HOME/.local/bin:$PATH"
export PATH="$HOME/bin:$PATH"

source ~/bin/ssh-agent.sh
source ~/bin/gpg-agent.sh

[ -f "$HOME/.bashrc" ] && . "$HOME/.bashrc"
