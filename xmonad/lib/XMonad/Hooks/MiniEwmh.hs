-- | Cribbed from XMonad.Hooks.EwmhDesktops, now doing things my way. :)

module XMonad.Hooks.MiniEwmh (
    mewmhSH, mewmhEH, mewmhLH
) where

import           XMonad
import qualified XMonad.StackSet          as S
import qualified XMonad.Hooks.UrgencyHook as XHU

import Data.Maybe

mewmhSH :: X ()
mewmhSH = withDisplay $ \dpy -> do
    r <- asks theRoot
    a <- getAtom "_NET_SUPPORTED"
    c <- getAtom "ATOM"
    supp <- mapM getAtom ["_NET_WM_STATE_DEMANDS_ATTENTION"
                         ,"_NET_ACTIVE_WINDOW"
                         ,"_NET_WM_STRUT"
                         ]
    io $ changeProperty32 dpy r a c propModeReplace (fmap fromIntegral supp)


mewmhEH :: Event -> X ()
mewmhEH ev@ClientMessageEvent {
               ev_window = w,
               ev_message_type = mt,
               ev_data = d
          } = withWindowSet $ \s -> do
    a_aw <- getAtom "_NET_ACTIVE_WINDOW"
    a_cw <- getAtom "_NET_CLOSE_WINDOW"
    a_da <- getAtom "_NET_WM_STATE_DEMANDS_ATTENTION"

    io $ putStrLn $ "EWMH EH: " ++ (show ev)

    maybe (return ()) (id) $ lookup mt
        [(a_aw, haw)
        ,(a_cw, hcw)
        ,(a_da, hda)
        ]
    return ()
 where
    hda = addUrgent
    haw = addUrgent
    hcw = killWindow w
    addUrgent = XHU.adjustUrgents (\ws -> if elem w ws then ws else w : ws)
mewmhEH _ = return ()

mewmhLH = do
    setActiveWindow

setActiveWindow :: X ()
setActiveWindow = withWindowSet $ \s -> withDisplay $ \dpy -> do
    let w = fromMaybe none (S.peek s)
    XHU.adjustUrgents (filter (/= w))
    r <- asks theRoot
    aw <- getAtom "_NET_ACTIVE_WINDOW"
    da <- getAtom "_NET_WM_STATE_DEMANDS_ATTENTION"
    cw <- getAtom "WINDOW"
    ca <- getAtom "ATOM"
    io $ changeProperty32 dpy r da ca propModeReplace []
    io $ changeProperty32 dpy r aw cw propModeReplace [fromIntegral w]
