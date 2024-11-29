# Executed by zsh(1) for interactive shells.

[[ $SHRC_DEBUG ]] && echo ~/.zshrc >&2

. ~/bin/term.sh

# ----------------------------------------

case $- in *i*) ;; *) return ;; esac

. ~/bin/shx.sh
. ~/bin/shrc-pre.sh

SAVEHIST=5000
TMPPREFIX=${TMPDIR:-/tmp}/zsh

typeset -gU path fpath cdpath

fpath=(
    ~/.zfunctions
    ~/projects/pub/pkgbuilds/.zsh
    $fpath
)

zmodload zsh/complist
zmodload zsh/datetime
zmodload zsh/parameter
zmodload zsh/system

autoload -Uz add-zle-hook-widget
autoload -Uz add-zsh-hook
autoload -Uz bashcompinit
autoload -Uz colors
autoload -Uz compinit
autoload -Uz copy-earlier-word
autoload -Uz edit-command-line
autoload -Uz run-help
autoload -Uz select-bracketed
autoload -Uz select-quoted
autoload -Uz zargs
autoload -Uz zmv

#setopt extended_glob
setopt always_to_end
setopt auto_cd
setopt auto_pushd
setopt auto_resume
setopt brace_ccl
setopt combining_chars
setopt complete_in_word
setopt extended_history
setopt glob_dots
setopt hist_expire_dups_first
setopt hist_find_no_dups
setopt hist_ignore_dups
setopt hist_ignore_space
setopt hist_lex_words # might be slow
setopt hist_reduce_blanks
setopt hist_verify
setopt inc_append_history_time
setopt interactive_comments
setopt long_list_jobs
setopt notify
setopt path_dirs
setopt prompt_subst
setopt pushd_ignore_dups
setopt pushd_silent
setopt pushd_to_home
setopt rc_quotes
setopt rm_star_silent
setopt vi

setopt share_history && unsetopt inc_append_history_time

unsetopt beep
unsetopt bg_nice
unsetopt check_jobs
unsetopt flow_control
unsetopt hist_beep
unsetopt hup
unsetopt list_beep
unsetopt multios
unsetopt nomatch

PS1='$__statstr:$HOSTNAME:${BASEDIR:+(${${BASEDIR##*/}//\%/%%}):}%1~%(!.#.$) '
if [[ $PIPENV_ACTIVE && $VIRTUAL_ENV ]]; then
    __venv=${VIRTUAL_ENV%/*}
    __venv=${__venv##*/}
    PS1="($__venv) $PS1"
fi
PS1=\$__vimode$PS1
[[ $terminfo[tsl] ]] && PS1=%{$terminfo[tsl]%n@%m:%~$terminfo[fsl]%}$PS1
PS2='$__vimode> '

__precmd() {
    local pstatus=($? $pipestatus) __cmd_dur
    __statstr=$pstatus[1]
    if (( $#pstatus > 2 )); then
        __statstr+=:${(j:|:)pstatus:1}
    fi
    setopt hist_ignore_dups
    [[ $__cmd_start ]] || return 0
    __cmd_dur=$((EPOCHSECONDS-__cmd_start))
    if (( __cmd_dur > 10 )); then
        printf '>>> %s (%ss): %s\n' \
           $(strftime %T $__cmd_start) $__cmd_dur $__cmd[1]
    fi
    unset __cmd __cmd_start
}
add-zsh-hook {,__}precmd

__preexec() {
    __cmd=("$@")
    __cmd_start=$EPOCHSECONDS
}
add-zsh-hook {,__}preexec

__zshaddhistory() {
    setopt no_hist_ignore_dups
    local line=${1%%$'\n'}
    [[ $line != (exit) ]]
}
add-zsh-hook {,__}zshaddhistory


# ZLE
# ----------------------------------------

KEYTIMEOUT=1
WORDCHARS=
#WORDCHARS='*?_-.[]~=/&;!#$%^(){}<>' (default)
ZLE_SPACE_SUFFIX_CHARS='&|'

zle_highlight=(
    isearch:bg=red,fg=231
    region:bg=236,fg=default
    special:fg=9,bold,standout
    suffix:fg=9,bold
    paste:standout
)

zle -C all-matches complete-word _generic
zle -N copy-earlier-word
zle -N edit-command-line
zle -N {,_}complete-help

__expand-aliases() {
    functions[___expand-aliases]=$BUFFER
    BUFFER=${functions[___expand-aliases]#$'\t'}
    CURSOR=$#BUFFER
}
zle -N {,__}expand-aliases

__expand-dot-to-parent-directory-path() {
    [[ $LBUFFER = *.. ]] && LBUFFER+=/.. || LBUFFER+=.
}
zle -N {,__}expand-dot-to-parent-directory-path

__expand-word-alias() {
    zle _expand_word
    zle _expand_alias
}
zle -N {,__}expand-word-alias

__noop() { }
zle -N {,__}noop

__reedit() {
    print -Rz - $PREBUFFER$BUFFER
    zle send-break
}
zle -N {,__}reedit

__toggle-comment-all() {
    local buf=$PREBUFFER$BUFFER nl=$'\n'
    if [[ $buf = \#* ]]; then
        buf=${buf##\#}
        buf=${buf//$nl\#/$nl}
    else
        buf=\#${buf//$nl/$nl\#}
    fi
    print -Rz - $buf
    zle send-break
}
zle -N {,__}toggle-comment-all

__zle-keymap-select() {
    __vimode=:
    if [[ ! $KEYMAP = vicmd ]]; then
        [[ $ZLE_STATE = *overwrite* ]] && __vimode=\^ || __vimode=+
    fi
    zle reset-prompt
}
zle -N {,__}zle-keymap-select

__zle-line-init() {
    zle zle-keymap-select
    zle set-local-history -n 1
}
zle -N {,__}zle-line-init

for i in backward-kill-line backward-kill-word kill-line kill-word; do
    eval "__$i-with-undo() { zle split-undo; zle $i; }"
    zle -N {,__}$i-with-undo
done
unset i

bindkey -rR \^A-\^_

for k in \\e\^{@.._} \\e{\ ..~}; do
    bindkey $k noop
    bindkey -M vicmd $k noop
done
for k in kdch1 kend kf{1..12} khome kich1 knp kpp; do
    case $k in
        kdch1) v=delete-char ;;
        kend) v=end-of-line ;;
        khome) v=beginning-of-line ;;
        *) v=noop ;;
    esac
    k=$terminfo[$k]
    [[ $k ]] || continue
    bindkey $k $v
    bindkey -M vicmd $k $v
done
unset k v

bindkey \^B beginning-of-line
bindkey \^E end-of-line
bindkey \\eh backward-char
bindkey \\el forward-char
bindkey \\eb backward-word # emacs-backward-word
bindkey \\ef emacs-forward-word
bindkey \\ew forward-word

bindkey \^? backward-delete-char
bindkey \^H backward-delete-char
bindkey \^U backward-kill-line-with-undo
bindkey \^K kill-line-with-undo
bindkey \^W backward-kill-word-with-undo
bindkey \\ed kill-word-with-undo
bindkey \\ex delete-char

bindkey \\e0 digit-argument
bindkey \\e1 digit-argument
bindkey \\e2 digit-argument
bindkey \\e3 digit-argument
bindkey \\e4 digit-argument
bindkey \\e5 digit-argument
bindkey \\e6 digit-argument
bindkey \\e7 digit-argument
bindkey \\e8 digit-argument
bindkey \\e9 digit-argument

bindkey \^A all-matches
bindkey \^D list-choices
bindkey \^I complete-word
bindkey \^O reverse-menu-complete
[[ $terminfo[kcbt] ]] && bindkey $terminfo[kcbt] reverse-menu-complete

bindkey \\e. insert-last-word
bindkey \^J self-insert
bindkey \^R history-incremental-search-backward
bindkey \^S history-incremental-search-forward
bindkey \^V quoted-insert
bindkey \^X\^A vi-cmd-mode
bindkey \^\[ vi-cmd-mode

bindkey ' ' magic-space
bindkey . expand-dot-to-parent-directory-path
bindkey \\ee expand-word-alias
bindkey \\em copy-earlier-word
bindkey \^XH _complete_help
bindkey \^Y toggle-comment-all
bindkey \^_ split-undo

bindkey \^G send-break
bindkey \^L clear-screen
bindkey \^M accept-line
bindkey \^P history-search-backward
bindkey \^N history-search-forward
bindkey \\e\< beginning-of-buffer-or-history
bindkey \\e\> end-of-buffer-or-history
bindkey \\ek up-line
bindkey \\ej down-line
bindkey \\eE expand-aliases
bindkey \\e\^M reedit
bindkey \^X\^E edit-command-line

bindkey -M vicmd \^G send-break
bindkey -M vicmd \^L clear-screen
bindkey -M vicmd \^M accept-line
bindkey -M vicmd \^P history-search-backward
bindkey -M vicmd \^N history-search-forward
bindkey -M vicmd \\e\< beginning-of-buffer-or-history
bindkey -M vicmd \\e\> end-of-buffer-or-history
bindkey -M vicmd \\ek up-line
bindkey -M vicmd \\ej down-line
bindkey -M vicmd \\eE expand-aliases
bindkey -M vicmd \\e\^M reedit
bindkey -M vicmd \^X\^E edit-command-line

bindkey -M vicmd \# toggle-comment-all
bindkey -M vicmd \\- vi-first-non-blank
bindkey -M vicmd \^J vi-open-line-below
bindkey -M vicmd \^R redo

bindkey -s \^Xp '^X^AA | pg'
bindkey -s \^XP '^X^AIpgx '
bindkey -s \^Xx '^X^A0isudo '
bindkey -s \^Xh "^X^Addihistory 25 | gi ''^X^Ai"
bindkey -s \^Xa '!!:*'
bindkey -s \^Xl '!!:$'
bindkey -s \^Xs '!!:gs/'
bindkey -s \^X{ '{,.}^X^Ai'
bindkey -s \^Xc '--color=auto '

bindkey -M vicmd -s \| 'A | '
bindkey -M vicmd -s \^Xp 'A | pg'
bindkey -M vicmd -s \^XP 'Ipgx '
bindkey -M vicmd -s \^Xx 'Isudo '
bindkey -M vicmd -s \^Xh "ddihistory 25 | gi ''^X^Ai"
bindkey -M vicmd -s \^Xa 'a!!:*'
bindkey -M vicmd -s \^Xl 'a!!:$'
bindkey -M vicmd -s \^Xs 'a!!:gs/'

bindkey -M isearch . self-insert
bindkey -M menuselect \\ej down-line-or-history
bindkey -M menuselect \\ek up-line-or-history
bindkey -M menuselect \^U send-break
bindkey -M visual \" quote-region
bindkey -M visual q deactivate-region

zle -N select-bracketed
for m in visual viopp; do
    for c in {a,i}${(s..)^:-'()[]{}<>bB'}; do
        bindkey -M $m $c select-bracketed
    done
done

zle -N select-quoted
for m in visual viopp; do
    for c in {a,i}{\',\",\`}; do
        bindkey -M $m $c select-quoted
    done
done


# Completion
# ----------------------------------------

if [[ ! $_comps ]]; then
    zcompdump=~/.cache/zcompdump
    compinit -d $zcompdump
    {
        if [[ ! -e $zcompdump.zwc || $zcompdump -nt $zcompdump.zwc ]]; then
            zcompile $zcompdump
        fi
    } &!
    bashcompinit
fi

# zshcompsys(1)
#
# :completion:<function>:<completer>:<command>:<argument>:<tag>
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

# See also:
#
#   ^X? _complete_debug
#   ^XH _complete_help

zstyle ':completion:*' completer _complete _match
zstyle ':completion:*' group-name ''
zstyle ':completion:*' insert-unambiguous true
zstyle ':completion:*' menu select
zstyle ':completion:*' squeeze-slashes true

zstyle ':completion:*:complete:*:*:*' cache-path ~/.cache/zcompcache
zstyle ':completion:*:complete:*:*:*' use-cache true

zstyle ':completion:*:*:(rm|kill|diff):*:*' ignore-line other
zstyle ':completion:*:*:-command-:*:*' group-order aliases reserved-words builtins functions commands
zstyle ':completion:*:*:-tilde-:*:*' group-order named-directories users
zstyle ':completion:*:*:cd:*:*' group-order path-directories local-directories
zstyle ':completion:*:*:kill:*:*' force-list always
zstyle ':completion:*:*:kill:*:*' insert-ids single
zstyle ':completion:*:*:kill:*:*' menu true select

zstyle ':completion:*:*:git:*' user-commands set-email set-upstream update

zstyle ':completion:*:*:*:*:default' list-colors ${(s.:.)LS_COLORS}
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
compctl -m srun
compctl -m torify
compctl -m torsocks
compctl -m watchx
compctl -v v

compdef f=find
compdef gitall=git
compdef xx=x

_pacpkgs() {
    local -a pkgs
    pkgs=(/var/lib/pacman/local/$1*(/))
    reply=(${${pkgs#/var/lib/pacman/local/}%-*-*})
}
compctl -K _pacpkgs \
    paccheck pacd pacdo paci pacl pacp pacrm pacw


# Aliases / Named directories
# ----------------------------------------

unalias run-help &>/dev/null
alias help=run-help

hash -d fonts=~/.local/share/fonts
hash -d journal=/var/log/journal
hash -d logs=/var/log
hash -d run=$XDG_RUNTIME_DIR
hash -d systemd-system=/etc/systemd/system
hash -d systemd-user=~/.config/systemd/user
hash -d udev.rules.d=/etc/udev/rules.d
hash -d xorg.conf.d=/etc/X11/xorg.conf.d


# Plugins
# ----------------------------------------

__plugin() {
    local name=$1 base
    declare -gA __plugin_loaded
    [[ $__plugin_loaded[$name] ]] && return
    for base in ~/opt /usr/share/zsh/plugins; do
        [[ -e $base/$name ]] || continue
        if [[ -e $base/$name/$name.plugin.zsh ]]; then
            . $base/$name/$name.plugin.zsh
        else
            . $base/$name/$name.zsh
        fi
        __plugin_loaded[$name]=1
        return $?
    done
    return 1
}

if __plugin zsh-autosuggestions; then
    ZSH_AUTOSUGGEST_ACCEPT_WIDGETS=(autosuggest-enable-accept)
    ZSH_AUTOSUGGEST_BUFFER_MAX_SIZE=20
    ZSH_AUTOSUGGEST_COMPLETION_IGNORE='rm *'
    ZSH_AUTOSUGGEST_HISTORY_IGNORE='rm *'
    ZSH_AUTOSUGGEST_MANUAL_REBIND=1
    ZSH_AUTOSUGGEST_PARTIAL_ACCEPT_WIDGETS=(emacs-forward-word-autosuggest)
    ZSH_AUTOSUGGEST_USE_ASYNC=1
    __autosuggest-enable-accept() {
        if (( $+_ZSH_AUTOSUGGEST_DISABLED )); then
            zle autosuggest-enable
        else
            zle autosuggest-accept
        fi
    }
    __emacs-forward-word-autosuggest() { zle emacs-forward-word; }
    zle -N {,__}autosuggest-enable-accept
    zle -N {,__}emacs-forward-word-autosuggest
    add-zle-hook-widget zle-line-init autosuggest-disable
    bindkey '^ ' autosuggest-enable-accept
    bindkey '\e ' emacs-forward-word-autosuggest
    bindkey '\e^ ' autosuggest-toggle
fi

if __plugin zsh-history-substring-search; then
    HISTORY_SUBSTRING_SEARCH_ENSURE_UNIQUE=1
    HISTORY_SUBSTRING_SEARCH_FUZZY=1
    HISTORY_SUBSTRING_SEARCH_GLOBBING_FLAGS=
    HISTORY_SUBSTRING_SEARCH_HIGHLIGHT_FOUND=bg=red,fg=231
    HISTORY_SUBSTRING_SEARCH_HIGHLIGHT_NOT_FOUND=
    bindkey \^P history-substring-search-up
    bindkey \^N history-substring-search-down
    bindkey -M vicmd \^P history-substring-search-up
    bindkey -M vicmd \^N history-substring-search-down
fi

if __plugin zsh-syntax-highlighting; then
    #ZSH_HIGHLIGHT_PATTERN+=(pattern style)
    #ZSH_HIGHLIGHT_REGEXP+=(pattern style)
    ZSH_HIGHLIGHT_HIGHLIGHTERS=(brackets main pattern regexp)
    ZSH_HIGHLIGHT_MAXLENGTH=50
    ZSH_HIGHLIGHT_STYLES[alias]=fg=14,bold
    ZSH_HIGHLIGHT_STYLES[assign]=fg=11
    ZSH_HIGHLIGHT_STYLES[back-double-quoted-argument]=fg=15
    ZSH_HIGHLIGHT_STYLES[builtin]=fg=11,bold
    ZSH_HIGHLIGHT_STYLES[command]=fg=10,bold
    ZSH_HIGHLIGHT_STYLES[commandseparator]=fg=15,bold
    ZSH_HIGHLIGHT_STYLES[dollar-double-quoted-argument]=fg=11
    ZSH_HIGHLIGHT_STYLES[function]=fg=14,bold
    ZSH_HIGHLIGHT_STYLES[globbing]=fg=11
    ZSH_HIGHLIGHT_STYLES[hashed-command]=fg=10,bold,underline
    ZSH_HIGHLIGHT_STYLES[history-expansion]=fg=14
    ZSH_HIGHLIGHT_STYLES[path]=fg=15
    ZSH_HIGHLIGHT_STYLES[precommand]=fg=11,bold,underline
    ZSH_HIGHLIGHT_STYLES[reserved-word]=fg=11,bold,underline
fi


# ----------------------------------------

. ~/bin/shrc-post.sh
