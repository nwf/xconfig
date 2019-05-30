xrandr --output LVDS1 --primary
urxvtd -q -o -f 2>/dev/null >/dev/null
xscreensaver -no-splash 2>/dev/null >/dev/null &
xset b off
inputplug -0 -c ~/lib/X/inputplug
# syndaemon -t -k -i 1 -d >/dev/null 2>/dev/null
# wicd-client 2>/dev/null >/dev/null &
# krandrtray 2>/dev/null >/dev/null &
export QT_QPA_PLATFORMTHEME=qt5ct
PATH=$HOME/bin:$PATH $HOME/.cabal/bin/xmonad

