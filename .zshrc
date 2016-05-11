# Executed by zsh(1) for interactive shells.

[[ $SHRC_DEBUG ]] && echo '~/.zshrc' >&2

. ~/bin/term.sh

# ----------------------------------------

case $- in *i*) ;; *) return ;; esac

. ~/bin/shx.sh
. ~/bin/shrc-pre.sh

HISTFILE="${ZDOTDIR:-$HOME}/.zhistory"
HISTSIZE=500
SAVEHIST=5000
TMPPREFIX="${TMPDIR:-/tmp}/zsh"

fpath=(~/.zfunctions $fpath)

typeset -gU path fpath cdpath

zmodload zsh/attr
zmodload zsh/complist
zmodload zsh/system
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


# Aliases
# ----------------------------------------

alias help='run-help'


# Window Title
# ----------------------------------------

function set-window-title {
    local title
    zformat -f title '%n@%m:%s' "s:${PWD/#$HOME/~}"
    printf '\e]2;%s\e\' "${(V%)title}"
}
add-zsh-hook precmd set-window-title


# ZLE
# ----------------------------------------

KEYTIMEOUT=1
WORDCHARS='!#$%&()*-;<>?[]^_{}~'
ZLE_SPACE_SUFFIX_CHARS='&|'

PROMPT='$__vimode%?$__statstr:${BASEDIR:+(${BASEDIR##*/}):}%1~%(!.#.$) '
if [[ $(hostname-label) ]]; then
    PROMPT='$__vimode%?$__statstr:%m:${BASEDIR:+(${BASEDIR##*/}):}%1~%(!.#.$) '
fi

function expand-dot-to-parent-directory-path {
    [[ $LBUFFER = *.. ]] && LBUFFER+='/..' || LBUFFER+='.'
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

function zle-line-init {
    # use global var since it's not possible to declare and assign local in one step
    __pstatus=("${pipestatus[@]}")
    __statstr=
    if (( ${#__pstatus[@]} > 1 )); then
        __statstr=":${__pstatus[1]}$(printf '|%s' "${__pstatus[@]:1}")"
    fi
    zle-keymap-select
}

function zle-keymap-select {
    __vimode=':'
    if [[ ! $KEYMAP = 'vicmd' ]]; then
        [[ $ZLE_STATE == *overwrite* ]] && __vimode='^' || __vimode='+'
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
if [[ ${key[Home]} ]]; then
    bindkey "${key[Home]}" beginning-of-line
    bindkey -M vicmd "${key[Home]}" beginning-of-line
fi

key[End]=${terminfo[kend]}
if [[ ${key[End]} ]]; then
    bindkey "${key[End]}" end-of-line
    bindkey -M vicmd "${key[End]}" end-of-line
fi

key[Insert]=${terminfo[kich1]}
if [[ ${key[Insert]} ]]; then
    bindkey "${key[Insert]}" overwrite-mode-select
    bindkey -M vicmd "${key[Insert]}" vi-insert
fi

key[Delete]=${terminfo[kdch1]}
if [[ ${key[Delete]} ]]; then
    bindkey "${key[Delete]}" delete-char
    bindkey -M vicmd "${key[Delete]}" delete-char
fi

key[PageUp]=${terminfo[kpp]}
if [[ ${key[PageUp]} ]]; then
    bindkey "${key[PageUp]}" noop
    bindkey -M vicmd "${key[PageUp]}" noop
fi

key[PageDown]=${terminfo[knp]}
if [[ ${key[PageDown]} ]]; then
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

bindkey -M viins -r '^X'
bindkey -M viins -s '^Xp' '\eIpgx \e0'
bindkey -M viins -s '^XP' '\eA | pg\eF|h'
bindkey -M viins -s '^Xx' '\e0isudo \e0'
bindkey -M viins -s '^Xh' "\eddihistory 25 | gi ''\ei"
bindkey -M viins -s '^Xa' '\ea!!:*\e'
bindkey -M viins -s '^Xl' '\ea!!:$\e'
bindkey -M viins -s '^Xs' '\ea!!:gs/'
bindkey -M viins -s '^Xc' '--color=auto '

bindkey -M vicmd -r '^X'
bindkey -M vicmd -s '^Xp' 'Ipgx \e0'
bindkey -M vicmd -s '^XP' 'A | pg\eF|h'
bindkey -M vicmd -s '^Xx' 'Isudo \e0'
bindkey -M vicmd -s '^Xh' "ddihistory 25 | gi ''\ei"
bindkey -M vicmd -s '^Xa' 'a!!:*\e'
bindkey -M vicmd -s '^Xl' 'a!!:$\e'
bindkey -M vicmd -s '^Xs' 'a!!:gs/'


# Completion
# ----------------------------------------

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

zstyle ':completion::*:(-command-|export):*' fake-parameters ${${${_comps[(I)-value-*]#*,}%%,*}:#-*-}

zstyle -e ':completion:*:hosts' hosts 'reply=(
    ${=${=${=${${(f)"$(cat {/etc/ssh_,~/.ssh/known_}hosts(|2)(N) 2>/dev/null)"}%%[#| ]*}//\]:[0-9]*/ }//,/ }//\[/ }
    ${=${(f)"$(cat /etc/hosts(|)(N) <<(ypcat hosts 2>/dev/null))"}%%\#*}
    ${=${${${${(@M)${(f)"$(cat ~/.ssh/config 2>/dev/null)"}:#Host *}#Host }:#*\**}:#*\?*}}
)'

zstyle ':completion:*:(rm|kill|diff):*' ignore-line other
zstyle ':completion:*:rm:*' file-patterns '*:all-files'

zstyle ':completion:*:*:*:*:processes' command 'ps -u $USER -o pid,user,comm -w'
zstyle ':completion:*:*:kill:*' force-list always
zstyle ':completion:*:*:kill:*' insert-ids single
zstyle ':completion:*:*:kill:*' menu yes select
zstyle ':completion:*:*:kill:*:processes' list-colors '=(#b) #([0-9]#) ([0-9a-z-]#)*=01;36=0=01'

zstyle ':completion:*:(scp|rsync):*' group-order users files all-files hosts-domain hosts-host hosts-ipaddr
zstyle ':completion:*:(scp|rsync):*' tag-order 'hosts:-host:host hosts:-domain:domain hosts:-ipaddr:ip\ address *'
zstyle ':completion:*:(ssh|scp|rsync):*:hosts-domain' ignored-patterns '<->.<->.<->.<->' '^[-[:alnum:]]##(.[-[:alnum:]]##)##' '*@*'
zstyle ':completion:*:(ssh|scp|rsync):*:hosts-host' ignored-patterns '*(.|:)*' loopback ip6-loopback localhost ip6-localhost broadcasthost
zstyle ':completion:*:(ssh|scp|rsync):*:hosts-ipaddr' ignored-patterns '^(<->.<->.<->.<->|(|::)([[:xdigit:].]##:(#c,2))##(|%*))' '127.0.0.<->' '255.255.255.255' '::1' 'fe80::*'
zstyle ':completion:*:ssh:*' group-order users hosts-domain hosts-host hosts-ipaddr
zstyle ':completion:*:ssh:*' tag-order 'hosts:-host:host hosts:-domain:domain hosts:-ipaddr:ip\ address *'

compctl -F fn
compctl -FBmwa i
compctl -f paco
compctl -k "(10m 15m 20m 25m 30m)" a
compctl -m on
compctl -m pacoc
compctl -m pgx
compctl -m pth
compctl -m ptha
compctl -m rep
compctl -m socksify
compctl -m torify
compctl -m torsocks
compctl -m watch
compctl -m x
compctl -m xx
compctl -v v

function _pacl {
    local -a packages
    read -cA words
    if [ "${#words}" -eq 2 ]; then
        packages=(/var/lib/pacman/local/"$1"*(/))
    fi
    reply=(${${packages#/var/lib/pacman/local/}%-*-*})
}
compctl -K _pacl pacl pacd pacp pacw paci pkgmark

function _mkvirtualenv-pyenv {
    local -a versions
    read -cA words
    if [ "${#words}" -eq 2 ]; then
        versions=($PYENV_ROOT/versions/"$1"*(/))
    fi
    reply=(${${versions#$PYENV_ROOT/versions/}%-*-*})
}
compctl -K _mkvirtualenv-pyenv mkvirtualenv-pyenv

function _systemd-dot {
    reply=(${${(f)"$(systemctl --full --no-legend --no-pager list-units --all)"}%% *})
}
compctl -K _systemd-dot systemd-dot


# Plugin: zsh-syntax-highlighting
# ----------------------------------------

__src=/usr/share/zsh/plugins/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh
if [[ -e $__src ]]; then
    . "$__src"
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
unset __src


# Plugin: zsh-history-substring-search
# ----------------------------------------

__src=~/opt/zsh-history-substring-search/zsh-history-substring-search.zsh
if [[ -e "$__src" ]]; then
    . "$__src"
    HISTORY_SUBSTRING_SEARCH_HIGHLIGHT_FOUND='bg=8,fg=15'
    HISTORY_SUBSTRING_SEARCH_HIGHLIGHT_NOT_FOUND='bg=red,fg=15'
fi
unset __src


# ----------------------------------------

. ~/bin/shrc-post.sh
