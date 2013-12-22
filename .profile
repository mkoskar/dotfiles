# ~/.profile
# - executed by the command interpreter for login shells
# - not read by bash(1), if ~/.bash_profile or ~/.bash_login exists

export PATH="$HOME/.local/bin:$PATH"
export PATH="$HOME/bin:$PATH"

source ~/bin/ssh-agent.sh
source ~/bin/gpg-agent.sh

[ -f "$HOME/.bashrc" ] && . "$HOME/.bashrc"
