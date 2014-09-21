# Source this file to initialize ssh-agent.
# :Compatibility: POSIX

info="$HOME/.ssh/ssh-agent-info"

(
    [ -e "$info" ] && . "$info"

    if [ -z "$SSH_AGENT_PID" ] ||
            ! ps -up "$SSH_AGENT_PID" &>/dev/null; then
        ssh-agent | grep '^SSH' >|"$info"
        chmod 600 "$info"
    fi
)

. "$info"
unset info
