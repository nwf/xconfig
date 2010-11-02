--------------------------------------------------------------- Headers {{{

{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE ScopedTypeVariables #-}

import           XMonad
import qualified XMonad.Actions.CycleWS as CWS
import qualified XMonad.Actions.Warp as AW
import qualified XMonad.Hooks.DynamicLog as HDL
import qualified XMonad.Hooks.ManageDocks as HMD
import qualified XMonad.Hooks.ManageHelpers as HMH
import qualified XMonad.Layout as L
import qualified XMonad.Layout.ResizableTile as LRT
import qualified XMonad.Prompt as XP
import qualified XMonad.Prompt.Window as XPW
import qualified XMonad.StackSet as SS
import qualified XMonad.Util.Cursor as XUC
import qualified XMonad.Util.ExtensibleState as XUE
import qualified XMonad.Util.EZConfig as EZC
import qualified XMonad.Util.Run as XUR
import qualified XMonad.Util.WindowProperties as XUW

import Data.Bits ((.|.))
import Data.Ratio ((%))
import Control.Monad (ap,liftM)
import System.IO (Handle, hPutStrLn, stderr)
import System.Posix.Process (executeFile)
import System.Posix.Signals (signalProcess, keyboardSignal)
import System.Posix.Types (ProcessID)

import Graphics.X11.Xinerama (getScreenInfo)

----------------------------------------------------------------------- }}}
----------------------------------------------------- Utility functions {{{

killpid :: ProcessID -> X ()
killpid = io . signalProcess keyboardSignal

getScreenCount :: X Int
getScreenCount = liftM length $ withDisplay $ io . getScreenInfo

----------------------------------------------------------------------- }}}
----------------------------------------------------- XMobar management {{{

data XMobars = XMobars [Maybe (Handle, ProcessID)] deriving Typeable
instance ExtensionClass XMobars where initialValue = XMobars []

respawnxmobars :: X ()
respawnxmobars = do
  screencount <- getScreenCount
  XMobars cxmbars <- XUE.get
  let (stay, tokill) = splitAt screencount cxmbars
  mapM_ (maybe (return ()) (killpid . snd)) tokill
  new <- mapM (\ix -> liftM Just . XUR.spawnPipePid $
                        "xmobar -x " ++ (show ix) ++ " " ++ "$HOME/lib/X/xmobarrc")
              [(length stay)..(screencount-1)]
  XUE.put . XMobars $ stay ++ new

killxmobars :: X ()
killxmobars = XUE.get >>= \(XMobars n) ->
              mapM_ (maybe (return ()) (killpid . snd)) n

----------------------------------------------------------------------- }}}
------------------------------------------------------- Management hook {{{

-- isKDEOverride = do
--    isover <- HMH.isInProperty "_NET_WM_WINDOW_TYPE" "_KDE_NET_WM_WINDOW_TYPE_OVERRIDE"
--    isfs <- HMH.isFullscreen
--    return $! isover && (not isfs)

myManageHook = composeAll . concat $
	[ [ className   =? c --> doFloat           | c <- myClassFloats]
	, [ title       =? t --> doFloat           | t <- myTitleFloats]
	, [ okularQuery --> doF (SS.shift "5") ]
	, [ className   =? "Iceweasel" --> doF (SS.shift "4") ]
	, [ HMH.composeOne [ HMH.isFullscreen HMH.-?> HMH.doFullFloat ] ]
	-- , [ HMH.composeOne [ isKDEOverride HMH.-?> doFloat ] ]
	]
 where
		-- This grabs only Okular root windows, not any dialogs they throw
		-- up.  Since I occasionally move Okulars to other workspaces, this
		-- is handy.
   okularQuery = XUW.propertyToQuery $
		(XUW.ClassName "Okular") `XUW.And` (XUW.Role "okular::Shell")
   myClassFloats = ["XVkbd"]
   myTitleFloats = ["KCharSelect"]

----------------------------------------------------------------------- }}}
----------------------------------------------------- Keyboard handling {{{

type Key = (KeyMask, KeySym)

delKeys :: XConfig l -> [Key]
delKeys conf@(XConfig {modMask = modm}) = []

addKeys :: XConfig l -> [(Key, X ())]
addKeys conf@(XConfig {modMask = modm}) =
    [
	    -- mod-0 %! Toggle to the workspace displayed previously
      ((modm, xK_0    ), CWS.toggleWS)
        -- mod-- %! Switch to the previous workspace
    , ((modm, xK_minus), CWS.prevWS  )
        -- mod-= %! Switch to the next workspace
    , ((modm, xK_equal), CWS.nextWS  )
		-- mod-a %! Warp to top left of currently focused window
    , ((modm, xK_a    ), AW.warpToWindow (1%10) (1%10))
		-- mod-b %! Toggle Struts
    , ((modm, xK_b), sendMessage HMD.ToggleStruts)
        -- mod-f %! Pull up Bring menu
    , ((modm, xK_f    ), XPW.windowPromptBring XP.defaultXPConfig)
        -- mod-g %! Pull up Goto menu
    , ((modm, xK_g    ), XPW.windowPromptGoto  XP.defaultXPConfig)
        -- mod-G %! Pull up Goto menu filtered for active workspace
    , ((modm .|. shiftMask, xK_g    ), XPW.windowPromptGotoCurrent  XP.defaultXPConfig)
		-- mod-o %! Pull up chraracter selector
	, ((modm .|. shiftMask, xK_o    ), spawn "kcharselect")
        -- XF86ScreenSaver or XF86PowerOff lock the screen
	, ((0, 0x1008ff2d ), xsl)
	, ((0, 0x1008ff2a ), xsl)
		-- for ResizableTall layouts
    , ((modm .|. shiftMask, xK_l ), sendMessage LRT.MirrorShrink)
    , ((modm .|. shiftMask, xK_h ), sendMessage LRT.MirrorExpand)
		-- 
	]
  where
   xsl = spawn "xscreensaver-command -lock"

----------------------------------------------------------------------- }}}
------------------------------------------------------------------ Main {{{

main = do
	-- Spawn these here so that the right thing happens when we restart
	-- xmonad after changing the number of displays.  Note that we grab
	-- the PIDs so we can kill the processes in the shutdown hook below.
	--
	-- See respawnxmobars below, too.
  {- trayp <- xfork $ executeFile "trayer" True
             [ "--edge", "top",
               "--align", "right",
               "--SetDockType", "true",
               "--SetPartialStrut", "true",
               "--expand", "false",
               "--height", "16",
               "--width", "75",
               "--widthtype", "pixels"
             ] Nothing
  trayp <- xfork $ executeFile "stalonetray" True
             [ "-i", "16"
             , "-d", "none"
             , "--geometry", "3x1+"++("0")++"+0"
             , "--window-strut", "top"
             ] Nothing
  -}
  xmonad $ customKeys defaultConfig
      { modMask = mod4Mask
      , terminal = "urxvtcd"
      , shutdownHook = do
            -- io $ signalProcess keyboardSignal trayp
            killxmobars
      , startupHook = do
            XUC.setDefaultCursor XUC.xC_left_ptr
            respawnxmobars
      , manageHook = manageHook defaultConfig <+> HMD.manageDocks <+> myManageHook
      , logHook = do
           XMobars cxmbars <- XUE.get
           HDL.dynamicLogWithPP $ HDL.xmobarPP
                   { HDL.ppOutput = \o ->
                       mapM_ (maybe (return ())
                                    (\(h,_) -> hPutStrLn h o))
                             cxmbars
                   , HDL.ppTitle = HDL.xmobarColor "green" "" . HDL.shorten 20
                   }
      , layoutHook = HMD.avoidStruts
                           $ LRT.ResizableTall 1 (3/100) (1/2) []
                         ||| L.Mirror (L.Tall 1 (3/100) (1/2))
                         ||| L.Full
      }
 where
    customKeys = (EZC.additionalKeys `ap` addKeys)
             . (EZC.removeKeys `ap` delKeys)

----------------------------------------------------------------------- }}}

-- TODO
-- Urgency hooks

-- VIM modeline, huzzah
-- vim: tw=76 ts=4 expandtab nu foldmethod=marker
