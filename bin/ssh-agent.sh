# Source this file to initialize ssh-agent.
# :Compatibility: POSIX

[ "$SSH_TTY" ] && return
hash ssh-agent &>/dev/null || return

info="$HOME/.ssh/ssh-agent-info"

(
    [ -e "$info" ] && . "$info"

    if [ ! "$SSH_AGENT_PID" ] ||
            ! ps -up "$SSH_AGENT_PID" >/dev/null 2>&1; then
        ssh-agent | grep '^SSH' >|"$info"
        chmod 600 "$info"
    fi
)

. "$info"
unset info
