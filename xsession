xmodmap $HOME/lib/X/xmodmap
urxvtd -q -o -f 2>/dev/null >/dev/null
xscreensaver -no-splash 2>/dev/null >/dev/null &
# wicd-client 2>/dev/null >/dev/null &
# krandrtray 2>/dev/null >/dev/null &
$HOME/.cabal/bin/xmonad

