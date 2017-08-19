# Common utilities.
# :Compatibility: Bash

init() {
    set -Eu -o pipefail
    shopt -s extglob

    trap_err() {
        local retstat=$?
        printf '%s: line %d: %s: (%d)\n' \
            "${BASH_SOURCE[1]}" "${BASH_LINENO[0]}" "$BASH_COMMAND" "$retstat"
        (( BASH_SUBSHELL )) && exit $retstat || exit 1
    } >&2
    trap trap_err ERR
}

initx() {
    set -Eu -o pipefail
    shopt -s extdebug extglob

    ti_3=
    ti_15=
    ti_error=
    ti_reset=

    if [[ -t 2 ]]; then
        ti_3=$(tput setaf 3)
        ti_15=$(tput setaf 15)
        ti_error=$(tput -S <<<$'setaf 15\nsetab 1')
        ti_reset=$(tput sgr0)
    fi

    trap_err() {
        local retstat=$?
        printf "\n${ti_error}ERR (%d): %s${ti_reset}\n" "$retstat" "$BASH_COMMAND"
        stacktrace "$retstat"
        (( BASH_SUBSHELL )) && exit $retstat || exit 1
    } >&2
    trap trap_err ERR

    stacktrace() {
        local retstat=${1:-$?} argvo argc i j
        local cmd=$BASH_COMMAND
        shopt -q extdebug || echo '> WARN: extdebug not set'
        declare -i len=${#FUNCNAME[@]}
        for (( i=0, argvo=0; i<len; i++, argvo+=argc )); do
            argc=${BASH_ARGC[$i]:-0}
            (( i > 0 )) || continue
            if (( i == len-1 )); then
                printf '%s' "${BASH_SOURCE[$i]}"
            else
                printf "%s:${ti_3}%d${ti_reset}: " \
                    "${BASH_SOURCE[$((i+1))]}" \
                    "${BASH_LINENO[$i]}"
                (( i == 1 )) || cmd=${FUNCNAME[$i]}
                printf "${ti_15}%s${ti_reset}" "$cmd"
            fi
            for (( j=argc-1; j>=0; j-- )); do
                printf "${ti_15} %s${ti_reset}" "${BASH_ARGV[$((argvo+j))]@Q}"
            done
            echo
        done
    }
}
