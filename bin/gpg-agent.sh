# Source this file to initialize gpg-agent.
# :Compatibility: POSIX

[ -n "$SSH_TTY" ] && return
command -v gpg-agent >/dev/null || return

info="$HOME/.gnupg/gpg-agent-info"

(
    [ -e "$info" ] && . "$info"

    if [ -z "$GPG_AGENT_INFO" ] ||
            ! ps -up "$(echo "$GPG_AGENT_INFO" | cut -d: -f2)" \
            >/dev/null 2>&1; then
        gpg-agent -q --daemon --log-file="$TMPDIR/gpg-agent.log" >|"$info"
        chmod 600 "$info"
    fi
)

. "$info"
unset info
