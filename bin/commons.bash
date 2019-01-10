# Common utilities.
# :Compatibility: Bash

# set -Eu -o pipefail
# shopt -s extglob
#
# PS4='+ ${BASH_SOURCE[0]}:$LINENO:${FUNCNAME[0]:+${FUNCNAME[0]}:}$?(${PIPESTATUS[*]}): '
#
# trap_err() {
#     local pstatus=($? "${PIPESTATUS[@]}")
#     local cmd=$BASH_COMMAND
#     local stat=${pstatus[0]}
#     if (( ${#pstatus[@]} > 2 )); then
#         cmd='<pipeline>'
#         stat+=:${pstatus[1]}$(printf '|%d' "${pstatus[@]:2}")
#     fi
#     printf '%s: line %d: %s: (%s)\n' "${BASH_SOURCE[1]}" \
#         "${BASH_LINENO[0]}" "$cmd" "$stat"
#     kill 0
# } >&2
# trap trap_err ERR

if [[ -o xtrace ]]; then
    set +x
    xtrace=1
    echo -------------------------------------------------- >&2
fi

set -Eu -o pipefail
shopt -s extglob

PS4='+ ${BASH_SOURCE[0]}:$LINENO:${FUNCNAME[0]:+${FUNCNAME[0]}:}$?(${PIPESTATUS[*]}): '

style_cmd=
style_error=
style_lineno=
style_none=
style_path=

if [[ -t 2 ]]; then
    style_cmd=$(tput setaf 11)
    style_error=$(tput -S <<<$'setaf 15\nsetab 1')
    style_lineno=$(tput setaf 3)
    style_none=$(tput sgr0)
fi

trap_err() {
    local pstatus=($? "${PIPESTATUS[@]}")
    set +x
    local cmd=$BASH_COMMAND
    local stat=${pstatus[0]}
    if (( ${#pstatus[@]} > 2 )); then
        cmd='<pipeline>'
        stat+=:${pstatus[1]}$(printf '|%d' "${pstatus[@]:2}")
    fi
    if shopt -q extdebug; then
        printf "\n${style_error}ERR(%s): %s${style_none}\n" "$stat" "$cmd"
        stacktrace
    else
        printf '%s: line %d: %s: (%s)\n' "${BASH_SOURCE[1]}" \
            "${BASH_LINENO[0]}" "$cmd" "$stat"
    fi
    kill 0
} >&2
trap trap_err ERR

stacktrace() {
    local argvo argc i j
    shopt -q extdebug || echo '> WARN: extdebug not set'
    declare -i len=${#FUNCNAME[@]}
    for (( i=0, argvo=0; i<len; i++, argvo+=argc )); do
        argc=${BASH_ARGC[$i]:-0}
        (( i > 0 )) || continue
        if (( i == len-1 )); then
            (( ! BASH_SUBSHELL )) || continue
            printf "${style_path}%s${style_none}" "${BASH_SOURCE[$i]}"
        else
            printf "${style_path}%s${style_none}:" "${BASH_SOURCE[$((i+1))]}"
            printf "${style_lineno}%d${style_none}: " "${BASH_LINENO[$i]}"
            printf "${style_cmd}%s${style_none}" "${FUNCNAME[$i]}"
        fi
        for (( j=argc-1; j>=0; j-- )); do
            printf "${style_cmd} %s${style_none}" "${BASH_ARGV[$((argvo+j))]@Q}"
        done
        echo
    done
}

[[ ! ${xtrace-} ]] || set -x
