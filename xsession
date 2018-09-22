xrandr --output LVDS1 --primary
urxvtd -q -o -f 2>/dev/null >/dev/null
xscreensaver -no-splash 2>/dev/null >/dev/null &
syndaemon -t -k -i 1 -d >/dev/null 2>/dev/null
xset b off
inputplug -0 -c ~/lib/X/inputplug
# wicd-client 2>/dev/null >/dev/null &
# krandrtray 2>/dev/null >/dev/null &
$HOME/.cabal/bin/xmonad

