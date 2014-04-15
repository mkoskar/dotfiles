# Source this file to initialize ssh-agent.
# :Compatibility: POSIX

info="$HOME/.ssh/ssh-agent-info"

(
    [ -f "$info" ] && . "$info"

    if [ -n "$SSH_AGENT_PID" ] &&
            ! ps -u -p "$SSH_AGENT_PID" >/dev/null 2>&1; then
        ssh-agent | grep '^SSH' >"$info"
        chmod 600 "$info"
    fi
)

. "$info"
unset info
