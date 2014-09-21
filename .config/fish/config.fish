set fish_greeting

function fish_prompt
    set -l vimode ':'
    switch $fish_bind_mode
        case insert
            set vimode '+'
        case visual
            set vimode '-'
    end
    printf '%s%s:%s$ ' "$vimode" "$status" (prompt_pwd)
end

function _fish_key_bindings
    fish_vi_key_bindings $argv

    bind \cl 'clear; commandline -f repaint'
    bind -M insert \cl 'clear; commandline -f repaint'
end

set -g fish_key_bindings _fish_key_bindings
