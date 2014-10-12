# ~/.zshrc
# - executed by zsh(1) for interactive shells

[ -f ~/bin/shrc.sh ] && . ~/bin/shrc.sh

# continue only in case of interactive shell
# ------------------------------------------
case $- in *i*) ;; *) return ;; esac

CDPATH='.:..:~'
HISTFILE="${ZDOTDIR:-$HOME}/.zhistory"
HISTSIZE=500
SAVEHIST=5000
TMPPREFIX="${TMPDIR:-/tmp}/zsh"

# ensure path arrays do not contain duplicates
typeset -gU path fpath cdpath

zmodload zsh/attr
zmodload zsh/complist
zmodload -F zsh/stat b:zstat

autoload -Uz add-zsh-hook
autoload -Uz compinit
autoload -Uz edit-command-line
autoload -Uz run-help
autoload -Uz url-quote-magic
autoload -Uz zargs
autoload -Uz zmv
autoload -Uz copy-earlier-word

setopt always_to_end
setopt auto_cd
setopt auto_pushd
setopt auto_resume
setopt brace_ccl
setopt cdable_vars
setopt combining_chars
setopt complete_in_word
setopt extended_glob
setopt extended_history
setopt hist_expire_dups_first
setopt hist_find_no_dups
setopt hist_ignore_all_dups
setopt hist_ignore_dups
setopt hist_ignore_space
setopt hist_save_no_dups
setopt hist_verify
setopt inc_append_history
setopt long_list_jobs
setopt multios
setopt notify
setopt path_dirs
setopt prompt_subst
setopt pushd_ignore_dups
setopt pushd_silent
setopt pushd_to_home
setopt rc_quotes
setopt share_history
setopt vi

unsetopt beep
unsetopt bg_nice
unsetopt check_jobs
unsetopt flow_control
unsetopt hist_beep
unsetopt hup

#========== completion

compinit

zstyle ':completion:*' completer _complete _match
zstyle ':completion:*' format ' %F{yellow}-- %d --%f'
zstyle ':completion:*' group-name ''
zstyle ':completion:*' matcher-list 'r:|[._-]=* r:|=*' 'l:|=* r:|=*'
zstyle ':completion:*' squeeze-slashes true
zstyle ':completion:*:*:*:*:*' menu select
zstyle ':completion:*:*:-subscript-:*' tag-order indexes parameters
zstyle ':completion:*:*:cd:*' tag-order local-directories directory-stack path-directories
zstyle ':completion:*:*:cd:*:directory-stack' menu yes select
zstyle ':completion:*:-tilde-:*' group-order named-directories path-directories users expand
zstyle ':completion:*:default' list-colors ${(s.:.)LS_COLORS}
zstyle ':completion:*:default' list-prompt '%S%M matches%s'
zstyle ':completion:*:descriptions' format ' %F{yellow}-- %d --%f'
zstyle ':completion:*:functions' ignored-patterns '(_*|pre(cmd|exec))'
zstyle ':completion:*:history-words' list false
zstyle ':completion:*:history-words' menu yes
zstyle ':completion:*:history-words' remove-all-dups yes
zstyle ':completion:*:history-words' stop yes
zstyle ':completion:*:manuals' separate-sections true
zstyle ':completion:*:manuals.(^1*)' insert-sections true
zstyle ':completion:*:match:*' original only
zstyle ':completion:*:matches' group 'yes'
zstyle ':completion:*:messages' format ' %F{purple}-- %d --%f'
zstyle ':completion:*:options' auto-description '%d'
zstyle ':completion:*:options' description 'yes'
zstyle ':completion:*:warnings' format ' %F{yellow}-- no matches found --%f'
zstyle ':completion::complete:*' cache-path "${ZDOTDIR:-$HOME}/.zcompcache"
zstyle ':completion::complete:*' use-cache on

# environment variables
zstyle ':completion::*:(-command-|export):*' fake-parameters ${${${_comps[(I)-value-*]#*,}%%,*}:#-*-}

# hosts
zstyle -e ':completion:*:hosts' hosts 'reply=(
    ${=${=${=${${(f)"$(cat {/etc/ssh_,~/.ssh/known_}hosts(|2)(N) 2>/dev/null)"}%%[#| ]*}//\]:[0-9]*/ }//,/ }//\[/ }
    ${=${(f)"$(cat /etc/hosts(|)(N) <<(ypcat hosts 2>/dev/null))"}%%\#*}
    ${=${${${${(@M)${(f)"$(cat ~/.ssh/config 2>/dev/null)"}:#Host *}#Host }:#*\**}:#*\?*}}
)'

# ignore multiple entries
zstyle ':completion:*:(rm|kill|diff):*' ignore-line other
zstyle ':completion:*:rm:*' file-patterns '*:all-files'

# kill
zstyle ':completion:*:*:*:*:processes' command 'ps -u $USER -o pid,user,comm -w'
zstyle ':completion:*:*:kill:*' force-list always
zstyle ':completion:*:*:kill:*' insert-ids single
zstyle ':completion:*:*:kill:*' menu yes select
zstyle ':completion:*:*:kill:*:processes' list-colors '=(#b) #([0-9]#) ([0-9a-z-]#)*=01;36=0=01'

# ssh / scp / rsync
zstyle ':completion:*:(scp|rsync):*' group-order users files all-files hosts-domain hosts-host hosts-ipaddr
zstyle ':completion:*:(scp|rsync):*' tag-order 'hosts:-host:host hosts:-domain:domain hosts:-ipaddr:ip\ address *'
zstyle ':completion:*:(ssh|scp|rsync):*:hosts-domain' ignored-patterns '<->.<->.<->.<->' '^[-[:alnum:]]##(.[-[:alnum:]]##)##' '*@*'
zstyle ':completion:*:(ssh|scp|rsync):*:hosts-host' ignored-patterns '*(.|:)*' loopback ip6-loopback localhost ip6-localhost broadcasthost
zstyle ':completion:*:(ssh|scp|rsync):*:hosts-ipaddr' ignored-patterns '^(<->.<->.<->.<->|(|::)([[:xdigit:].]##:(#c,2))##(|%*))' '127.0.0.<->' '255.255.255.255' '::1' 'fe80::*'
zstyle ':completion:*:ssh:*' group-order users hosts-domain hosts-host hosts-ipaddr
zstyle ':completion:*:ssh:*' tag-order 'hosts:-host:host hosts:-domain:domain hosts:-ipaddr:ip\ address *'

# custom
compctl -k "(10m 15m 20m 25m 30m)" a
compctl -m pth
compctl -m spth
compctl -m on
compctl -f paco
compctl -m pacoc

function _pacl {
    local -a packages_long
    packages_long=(/var/lib/pacman/local/"$1"*(/))
    reply=(${${packages_long#/var/lib/pacman/local/}%-*-*})
}
compctl -K _pacl pacl pacd pacp pacw paci

#========== window-title

function set-window-title {
    local title
    zformat -f title '%n@%m:%s' "s:${PWD/#$HOME/~}"
    printf '\e]0;%s\e\' "${(V%)title}"
}
add-zsh-hook precmd set-window-title

#========== zle

KEYTIMEOUT=1
WORDCHARS='*?_-.[]~&;!#$%^(){}<>'

PROMPT='$_vimode%?:%1~%(!.#.$) '
if [ -n "$(hostname-label)" ]; then
    PROMPT='$_vimode%?:%m:%1~%(!.#.$) '
fi

function expand-dot-to-parent-directory-path {
    [[ "$LBUFFER" = *.. ]] && LBUFFER+='/..' || LBUFFER+='.'
}

function expand-word-alias {
    zle expand-word
    zle _expand_alias
}

function noop {}

function overwrite-mode-select {
    zle overwrite-mode
    zle zle-keymap-select
}

function zle-line-init zle-keymap-select {
    _vimode=':'
    if [ ! "$KEYMAP" = 'vicmd' ]; then
        [[ "$ZLE_STATE" == *overwrite* ]] && _vimode='^' || _vimode='+'
    fi
    zle reset-prompt
}

zle -N copy-earlier-word
zle -N edit-command-line
zle -N expand-dot-to-parent-directory-path
zle -N expand-word-alias
zle -N noop
zle -N overwrite-mode-select
zle -N self-insert url-quote-magic
zle -N zle-keymap-select
zle -N zle-line-init

bindkey -v
bindkey -r '^A' '^B' '^C' '^F' '^Q' '^T' '^X' '^Y' '^Z' '^\' '^]' '^^' '^_'

typeset -A key

key[Home]=${terminfo[khome]}
if [ -n "${key[Home]}" ]; then
    bindkey "${key[Home]}" beginning-of-line
    bindkey -M vicmd "${key[Home]}" beginning-of-line
fi

key[End]=${terminfo[kend]}
if [ -n "${key[End]}" ]; then
    bindkey "${key[End]}" end-of-line
    bindkey -M vicmd "${key[End]}" end-of-line
fi

key[Insert]=${terminfo[kich1]}
if [ -n "${key[Insert]}" ]; then
    bindkey "${key[Insert]}" overwrite-mode-select
    bindkey -M vicmd "${key[Insert]}" vi-insert
fi

key[Delete]=${terminfo[kdch1]}
if [ -n "${key[Delete]}" ]; then
    bindkey "${key[Delete]}" delete-char
    bindkey -M vicmd "${key[Delete]}" delete-char
fi

key[PageUp]=${terminfo[kpp]}
if [ -n "${key[PageUp]}" ]; then
    bindkey "${key[PageUp]}" noop
    bindkey -M vicmd "${key[PageUp]}" noop
fi

key[PageDown]=${terminfo[knp]}
if [ -n "${key[PageDown]}" ]; then
    bindkey "${key[PageDown]}" noop
    bindkey -M vicmd "${key[PageDown]}" noop
fi

bindkey '^O' menu-complete
bindkey '^K' reverse-menu-complete
bindkey '^N' history-substring-search-down
bindkey '^P' history-substring-search-up
bindkey '^R' history-incremental-search-backward
bindkey '^S' history-incremental-search-forward
bindkey '^E' expand-word-alias
bindkey '^W' backward-kill-word
bindkey '^H' backward-delete-char
bindkey '^?' backward-delete-char
bindkey '.' expand-dot-to-parent-directory-path
bindkey '\e.' insert-last-word
bindkey '\eh' run-help
bindkey '\em' copy-earlier-word

bindkey -M vicmd '^N' history-substring-search-down
bindkey -M vicmd '^P' history-substring-search-up
bindkey -M vicmd '^R' redo
bindkey -M vicmd '^E' expand-word-alias
bindkey -M vicmd '^W' backward-kill-word
bindkey -M vicmd 'j' down-history
bindkey -M vicmd 'k' up-history
bindkey -M vicmd 'e' edit-command-line
bindkey -M vicmd 'u' undo
bindkey -M vicmd '\eh' run-help

bindkey -M isearch . self-insert

bindkey -M menuselect '^U' send-break

#========== zsh-syntax-highlighting

_src=/usr/share/zsh/plugins/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh
if [ -e "$_src" ]; then
    . "$_src"
    ZSH_HIGHLIGHT_HIGHLIGHTERS=(main brackets pattern)
    #ZSH_HIGHLIGHT_STYLES[default]='none'
    #ZSH_HIGHLIGHT_STYLES[unknown-token]='fg=red,bold'
    ZSH_HIGHLIGHT_STYLES[reserved-word]='fg=yellow,bold,underline'
    ZSH_HIGHLIGHT_STYLES[alias]='fg=cyan,bold'
    ZSH_HIGHLIGHT_STYLES[builtin]='fg=yellow,bold'
    ZSH_HIGHLIGHT_STYLES[function]='fg=cyan,bold'
    ZSH_HIGHLIGHT_STYLES[command]='fg=green,bold'
    ZSH_HIGHLIGHT_STYLES[precommand]='fg=yellow,bold,underline'
    ZSH_HIGHLIGHT_STYLES[commandseparator]='fg=white,bold'
    ZSH_HIGHLIGHT_STYLES[hashed-command]='fg=green,bold,underline'
    ZSH_HIGHLIGHT_STYLES[path]='fg=15'
    ZSH_HIGHLIGHT_STYLES[path_prefix]='none'
    ZSH_HIGHLIGHT_STYLES[path_approx]='none'
    ZSH_HIGHLIGHT_STYLES[globbing]='fg=11'
    ZSH_HIGHLIGHT_STYLES[history-expansion]='fg=14'
    #ZSH_HIGHLIGHT_STYLES[single-hyphen-option]='none'
    #ZSH_HIGHLIGHT_STYLES[double-hyphen-option]='none'
    #ZSH_HIGHLIGHT_STYLES[back-quoted-argument]='none'
    #ZSH_HIGHLIGHT_STYLES[single-quoted-argument]='fg=yellow'
    #ZSH_HIGHLIGHT_STYLES[double-quoted-argument]='fg=yellow'
    ZSH_HIGHLIGHT_STYLES[dollar-double-quoted-argument]='fg=11'
    ZSH_HIGHLIGHT_STYLES[back-double-quoted-argument]='fg=15'
    ZSH_HIGHLIGHT_STYLES[assign]='fg=11'
fi

#========== zsh-history-substring-search

_src=~/opt/zsh-plugins/zsh-history-substring-search/zsh-history-substring-search.zsh
if [ -e "$_src" ]; then
    . "$_src"
    HISTORY_SUBSTRING_SEARCH_HIGHLIGHT_FOUND='bg=8,fg=15'
    HISTORY_SUBSTRING_SEARCH_HIGHLIGHT_NOT_FOUND='bg=red,fg=15'
fi

#========== aliases

alias help='run-help'
