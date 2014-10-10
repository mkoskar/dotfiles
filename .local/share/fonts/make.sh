#!/bin/bash

set -e

cd ~/.local/share/fonts

get() {
    local fname=$(curl -sLJO -w '%{filename_effective}' "$1")
    local outd=$(mktemp -d)
    aunpack -X "$outd" "$fname"
    find "$outd" -iregex '.*\.\(ttf\|otf\)' -exec mv {} . \;
    rm -r "$fname" "$outd"
}

get http://www.fontsquirrel.com/fonts/download/Anonymous
get http://www.fontsquirrel.com/fonts/download/BPmono
get http://www.fontsquirrel.com/fonts/download/MonospaceTypewriter
get http://www.fontsquirrel.com/fonts/download/jura1
get http://www.fontsquirrel.com/fonts/download/lekton
get http://www.fontsquirrel.com/fonts/download/oxygen-mono
get http://www.fontsquirrel.com/fonts/download/pt-mono
