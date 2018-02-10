# Executed by zsh(1) for interactive shells.

[[ $SHRC_DEBUG ]] && echo '~/.zshrc' >&2

. ~/bin/term.sh

# ----------------------------------------

case $- in *i*) ;; *) return ;; esac

. ~/bin/shx.sh
. ~/bin/shrc-pre.sh

HISTFILE="$HOME/.local/share/zsh_history"
HISTSIZE=1000
SAVEHIST=5000
TMPPREFIX="${TMPDIR:-/tmp}/zsh"

fpath=(~/.zfunctions $fpath)

typeset -gU path fpath cdpath

zmodload zsh/complist
zmodload zsh/system

autoload -Uz add-zsh-hook
autoload -Uz bracketed-paste-magic
autoload -Uz compinit
autoload -Uz copy-earlier-word
autoload -Uz edit-command-line
autoload -Uz run-help
autoload -Uz zargs
autoload -Uz zmv

setopt always_to_end
setopt auto_cd
setopt auto_pushd
setopt auto_resume
setopt brace_ccl
setopt cdable_vars
setopt combining_chars
setopt complete_in_word
setopt extended_history
setopt glob_dots
setopt hist_expire_dups_first
setopt hist_find_no_dups
setopt hist_ignore_all_dups
setopt hist_ignore_dups
setopt hist_ignore_space
setopt hist_save_no_dups
setopt hist_verify
setopt inc_append_history
setopt interactive_comments
setopt long_list_jobs
setopt nomultios
setopt nonomatch
setopt notify
setopt path_dirs
setopt posix_builtins
setopt prompt_subst
setopt pushd_ignore_dups
setopt pushd_silent
setopt pushd_to_home
setopt rc_quotes
setopt rm_star_silent
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

unalias run-help &>/dev/null
alias help='run-help'


# Window Title
# ----------------------------------------

set-window-title() {
    local title
    zformat -f title '%n@%m:%s' "s:${PWD/#$HOME/~}"
    hstatus "${(V%)title}"
}
add-zsh-hook precmd set-window-title


# ZLE
# ----------------------------------------

KEYTIMEOUT=1
WORDCHARS=''
#WORDCHARS='*?_-.[]~=/&;!#$%^(){}<>'
ZLE_SPACE_SUFFIX_CHARS='&|'

PROMPT='$__vimode%?$__statstr:${BASEDIR:+(${BASEDIR##*/}):}%1~%(!.#.$) '
PROMPT2='$__vimode> '
if [[ $HOST != 'mirci' ]]; then
    PROMPT='$__vimode%?$__statstr:%m:${BASEDIR:+(${BASEDIR##*/}):}%1~%(!.#.$) '
fi

expand-dot-to-parent-directory-path() {
    [[ $LBUFFER = *.. ]] && LBUFFER+='/..' || LBUFFER+='.'
}

expand-word-alias() {
    zle expand-word
    zle _expand_alias
}

noop() { }

zle-keymap-select() {
    __vimode=':'
    if [[ ! $KEYMAP = 'vicmd' ]]; then
        [[ $ZLE_STATE == *overwrite* ]] && __vimode='^' || __vimode='+'
    fi
    zle reset-prompt
}

zle-line-init() {
    __pstatus=("${pipestatus[@]}")
    __statstr=
    if (( ${#__pstatus[@]} > 1 )); then
        __statstr=":${__pstatus[1]}$(printf '|%s' "${__pstatus[@]:1}")"
    fi
    zle zle-keymap-select
}

zle -C all-matches complete-word _generic
zle -N bracketed-paste bracketed-paste-magic
zle -N complete-help _complete_help
zle -N copy-earlier-word
zle -N edit-command-line
zle -N expand-dot-to-parent-directory-path
zle -N expand-word-alias
zle -N noop
zle -N zle-keymap-select
zle -N zle-line-init

bindkey -rR '^A-^_'

for k in {@.._}; do
    bindkey "\e^$k" noop
    bindkey -M vicmd "\e^$k" noop
done

for k in {\ ..~}; do
    bindkey "\e$k" noop
    bindkey -M vicmd "\e$k" noop
done

for k in kich1 kpp knp kf{1..12}; do
    k=${terminfo[$k]}
    bindkey "$k" noop
    bindkey -M vicmd "$k" noop
done

k=${terminfo[khome]}
bindkey "$k" beginning-of-line
bindkey -M vicmd "$k" beginning-of-line

k=${terminfo[kend]}
bindkey "$k" end-of-line
bindkey -M vicmd "$k" end-of-line

unset k

bindkey '^B' beginning-of-line
bindkey '^E' end-of-line
bindkey '\eh' backward-char
bindkey '\el' forward-char
bindkey '\eb' backward-word
bindkey '\ef' emacs-forward-word
bindkey '\ew' emacs-forward-word

bindkey '^U' backward-kill-line
bindkey '^K' kill-line
bindkey '^W' backward-kill-word
bindkey '\ed' kill-word
bindkey '\ex' delete-char
bindkey "${terminfo[kdch1]}" delete-char

bindkey '^A' all-matches
bindkey '^D' list-choices
bindkey '^O' reverse-menu-complete
bindkey "${terminfo[kcbt]}" reverse-menu-complete
bindkey '^I' complete-word

bindkey '^G' send-break
bindkey '^L' clear-screen
bindkey '^P' history-search-backward
bindkey '^N' history-search-forward
bindkey '\ek' up-line
bindkey '\ej' down-line

bindkey '\e0' digit-argument
bindkey '\e1' digit-argument
bindkey '\e2' digit-argument
bindkey '\e3' digit-argument
bindkey '\e4' digit-argument
bindkey '\e5' digit-argument
bindkey '\e6' digit-argument
bindkey '\e7' digit-argument
bindkey '\e8' digit-argument
bindkey '\e9' digit-argument

bindkey '^H' backward-delete-char
bindkey '^J' accept-line
bindkey '^M' accept-line
bindkey '^R' history-incremental-search-backward
bindkey '^S' history-incremental-search-forward
bindkey '^V' quoted-insert
bindkey '^[' vi-cmd-mode
bindkey '^X^A' vi-cmd-mode

bindkey ' ' magic-space
bindkey '\ee' expand-word-alias
bindkey '^X^E' edit-command-line

bindkey '.' expand-dot-to-parent-directory-path
bindkey '\e.' insert-last-word
bindkey '\em' copy-earlier-word
bindkey '\e^M' self-insert-unmeta
bindkey '^XH' _complete_help

bindkey -M vicmd 'k' up-history
bindkey -M vicmd 'j' down-history
bindkey -M vicmd 'u' undo
bindkey -M vicmd '^G' send-break
bindkey -M vicmd '^P' history-search-backward
bindkey -M vicmd '^N' history-search-forward
bindkey -M vicmd '^R' redo
bindkey -M vicmd '\ek' up-line
bindkey -M vicmd '\ej' down-line

bindkey -M vicmd '\ee' expand-word-alias
bindkey -M vicmd '^X^E' edit-command-line

bindkey -M isearch . self-insert

bindkey -M menuselect '^U' send-break

bindkey -s '^Xp' '^X^AIpgx '
bindkey -s '^XP' '^X^AA | pg'
bindkey -s '^Xx' '^X^A0isudo '
bindkey -s '^Xh' "^X^Addihistory 25 | gi ''^X^Ai"
bindkey -s '^Xa' '^X^Aa!!:*'
bindkey -s '^Xl' '^X^Aa!!:$'
bindkey -s '^Xs' '^X^Aa!!:gs/'
bindkey -s '^Xc' '--color=auto '

bindkey -M vicmd -s '^Xp' 'Ipgx '
bindkey -M vicmd -s '^XP' 'A | pg'
bindkey -M vicmd -s '^Xx' 'Isudo '
bindkey -M vicmd -s '^Xh' "ddihistory 25 | gi ''^X^Ai"
bindkey -M vicmd -s '^Xa' 'a!!:*'
bindkey -M vicmd -s '^Xl' 'a!!:$'
bindkey -M vicmd -s '^Xs' 'a!!:gs/'


# Completion
# ----------------------------------------

zcompdump="$HOME/.cache/zcompdump"
compinit -d "$zcompdump"
{
    if [[ ! -e "$zcompdump.zwc" || "$zcompdump" -nt "$zcompdump.zwc" ]]; then
        zcompile "$zcompdump"
    fi
} &!

# zshcompsys(1)
#
# > completion:<function>:<completer>:<command>:<argument>:<tag>
#
# function:
#   Function, if completion is called from a named widget rather than
#   through the normal completion system; blank otherwise.
#
# completer:
#   Completer currently active, the name of the function without the
#   leading underscore and with other underscores converted to hyphens;
#   see 'Control Functions'.
#
# command:
#   Command or a special -context-, just as it appears following the
#   #compdef tag or the compdef function.
#
# argument:
#   This indicates which command line or option argument we are completing.
#
# tag:
#   Used to discriminate between the types of matches a completion
#   function can generate in a certain context; see 'Standard Tags'.

zstyle ':completion:*' completer _complete _match
zstyle ':completion:*' group-name ''
zstyle ':completion:*' insert-unambiguous true
zstyle ':completion:*' menu select
zstyle ':completion:*' squeeze-slashes true

zstyle ':completion:*:complete:*:*:*' cache-path ~/.cache/zcompcache
zstyle ':completion:*:complete:*:*:*' use-cache true

zstyle ':completion:*:*:(rm|kill|diff):*:*' ignore-line other
zstyle ':completion:*:*:-command-:*:*' group-order aliases reserved-words builtins functions commands
zstyle ':completion:*:*:cd:*:*' group-order named-directories directory-stack path-directories local-directories
zstyle ':completion:*:*:kill:*:*' force-list always
zstyle ':completion:*:*:kill:*:*' insert-ids single
zstyle ':completion:*:*:kill:*:*' menu true select

zstyle ':completion:*:*:*:*:default' list-colors "${(s.:.)LS_COLORS}"
zstyle ':completion:*:*:*:*:default' list-prompt '%S%m%s'
zstyle ':completion:*:*:*:*:descriptions' format ' %F{yellow}-- %d --%f'
zstyle ':completion:*:*:*:*:hosts' use-ip true
zstyle ':completion:*:*:*:*:manuals' separate-sections true
zstyle ':completion:*:*:*:*:manuals.*' insert-sections true
zstyle ':completion:*:*:*:*:messages' format ' %F{yellow}-- %d --%f'
zstyle ':completion:*:*:*:*:processes' command 'ps -u $USER -o pid,user,comm -w'
zstyle ':completion:*:*:*:*:warnings' format ' %F{yellow}-- no matches found --%f'

zstyle -e ':completion:*:*:*:*:hosts' hosts 'reply=(
    ${=${(f)"$(cat /etc/hosts(N))"}%%\#*}
    ${=${${${${(@M)${(f)"$(cat ~/.ssh/config 2>/dev/null)"}:#Host *}#Host }:#*\**}:#*\?*}}
)'

zstyle ':completion:all-matches:*' completer _all_matches _complete
zstyle ':completion:all-matches:*' insert true

compctl -F fn
compctl -FBmwa i
compctl -f paco
compctl -k '(10m 15m 20m 25m 30m)' a
compctl -m on
compctl -m pacoc
compctl -m pgx
compctl -m pth
compctl -m rep
compctl -m socksify
compctl -m torify
compctl -m torsocks
compctl -m watch
compctl -v v

compdef gitall=git

_pacl() {
    local -a packages
    read -cA words
    if [ "${#words}" -eq 2 ]; then
        packages=(/var/lib/pacman/local/"$1"*(/))
    fi
    reply=(${${packages#/var/lib/pacman/local/}%-*-*})
}
compctl -K _pacl pacl pacd pacp pacw paci paccheck pacscripts

_mkvirtualenv_pyenv() {
    local -a versions
    read -cA words
    if [ "${#words}" -eq 2 ]; then
        versions=($PYENV_ROOT/versions/"$1"*(/))
    fi
    reply=(${${versions#$PYENV_ROOT/versions/}%-*-*})
}
compctl -K _mkvirtualenv_pyenv mkvirtualenv_pyenv

_systemd_dot() {
    reply=(${${(f)"$(systemctl --full --no-legend --no-pager list-units --all)"}%% *})
}
compctl -K _systemd_dot systemd_dot

_xsession() {
    reply=(${(f)"$(xsession '-?')"})
}
compctl -K _xsession x xx


# Plugin: zsh-syntax-highlighting
# ----------------------------------------

__src=~/opt/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh
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

    bindkey '^P' history-substring-search-up
    bindkey '^N' history-substring-search-down
    bindkey -M vicmd '^P' history-substring-search-up
    bindkey -M vicmd '^N' history-substring-search-down
fi
unset __src


# ----------------------------------------

. ~/bin/shrc-post.sh
