#!/bin/sh

xindevno=$(xinput list | grep TouchPad | cut -d '=' -f 2 | grep -e "^[0-9]*" | cut -f 1 )
# echo "Trackpad appears to be device ${xindevno}"
xinput set-prop ${xindevno} "Device Enabled" "$@"
