# Source this file to initialize "gpg-agent".

INFO="$HOME/.gnupg/gpg-agent-info"

(
    [ -f "$INFO" ] && source "$INFO"

    if [ -z "$GPG_AGENT_INFO" ] || ! ps -u -p $(echo "$GPG_AGENT_INFO" | cut -d: -f 2) > /dev/null; then
        gpg-agent -q --daemon > "$INFO"
        #gpg-agent -q --daemon --enable-ssh-support > "$INFO"
        chmod 600 "$INFO"
    fi
)

source "$INFO"