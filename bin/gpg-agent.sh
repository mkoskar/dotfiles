# Source this file to initialize gpg-agent.
# :Compatibility: POSIX

info="$HOME/.gnupg/gpg-agent-info"

(
    [ -f "$info" ] && . "$info"

    if [ -n "$GPG_AGENT_INFO" ] &&
            ! ps -u -p "$(echo "$GPG_AGENT_INFO" | cut -d: -f 2)" \
            >/dev/null 2>&1; then
        gpg-agent -q --daemon >"$info"
        chmod 600 "$info"
    fi
)

. "$info"
unset info
