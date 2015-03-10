# Source this file to initialize DISPLAY environment variable if it is empty.
# :Compatibility: POSIX

if [ -z "$DISPLAY" ]; then
    export DISPLAY=$(xdisplay)
fi
