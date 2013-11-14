# Source this file to initialize "ssh-agent".

INFO="$HOME/.ssh/ssh-agent-info"

(
    [ -f "$INFO" ] && source "$INFO"

    if [ -z "$SSH_AGENT_PID" ] || ! ps -u -p $SSH_AGENT_PID > /dev/null; then
        ssh-agent | grep ^SSH > "$INFO"
        chmod 600 "$INFO"
    fi
)

source "$INFO"
