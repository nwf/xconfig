--------------------------------------------------------------- Headers {{{

{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DoRec #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

import           XMonad
-- import qualified XMonad.Core as C
import qualified XMonad.Actions.CopyWindow as CW
import qualified XMonad.Actions.CycleWS as CWS
-- import qualified XMonad.Actions.DynamicWorkspaces as ADW
import qualified XMonad.Actions.Eval as AE
import qualified XMonad.Actions.Submap as AS
import qualified XMonad.Actions.Warp as AW
import qualified XMonad.Actions.WithAll as AWA
import qualified XMonad.Actions.XMobars as AXB
import qualified XMonad.Hooks.ManageDocks as HMD
import qualified XMonad.Hooks.ManageHelpers as HMH
import qualified XMonad.Hooks.UrgencyHook as HUH
import qualified XMonad.Layout as L
import qualified XMonad.Layout.IndependentScreens as LIS
import qualified XMonad.Layout.LayoutHints as LLH
import qualified XMonad.Layout.MultiColumns as LMC
import qualified XMonad.Layout.PerWorkspace as LPW
import qualified XMonad.Layout.ResizableTile as LRT
import qualified XMonad.Layout.SLS as LS 
import qualified XMonad.Prompt as P
import qualified XMonad.Prompt.Eval as PE
-- import qualified XMonad.Prompt.Input as PI
import qualified XMonad.Prompt.Window as PW
import qualified XMonad.StackSet as S
import qualified XMonad.Util.Cursor as UC
import qualified XMonad.Util.ExtensibleState as UE
import qualified XMonad.Util.EZConfig as EZC
import qualified XMonad.Util.WindowProperties as UW


import Control.Monad (ap,liftM2,when)
import Data.Maybe (isNothing)
import qualified Data.Map as M
import Data.List (find, isPrefixOf, stripPrefix)
import Data.Monoid (All(All),mappend)
import Data.Ratio ((%))
-- import qualified Language.Haskell.TH as TH
-- import System.FilePath ((</>))
import System.IO (hPutStrLn, stderr)
-- import System.Posix.Directory (getWorkingDirectory)
-- import System.Posix.Process (getProcessStatus, ProcessStatus(..))
import System.Posix.Signals (signalProcess, keyboardSignal)
import System.Posix.Types (ProcessID)

-- import Graphics.X11.Xinerama (getScreenInfo)

----------------------------------------------------------------------- }}}
----------------------------------------------------- Utility functions {{{

killpid :: MonadIO m => ProcessID -> m ()
killpid = io . signalProcess keyboardSignal

-- In here due to the apparent lack of a replace function in the standard
-- library.  (Used for correctly displaying newlines in error messages)
-- Stolen from Actions.Eval
replace :: Eq a => [a] -> [a] -> [a] -> [a]
replace lst@(x:xs) sub repl | sub `isPrefixOf` lst = repl ++ replace
                                                     (drop (length sub) lst) sub repl
                            | otherwise = x:(replace xs sub repl)
replace _ _ _ = []

-- | Find the screen which is displaying a given workspace tag
findScreenByTag :: WorkspaceId
                -> X (Maybe
                       (S.Screen WorkspaceId (Layout Window)
                                 Window ScreenId ScreenDetail))
findScreenByTag i = gets (S.screens . windowset) 
                  >>= return . find ((== i) . (S.tag . S.workspace))

----------------------------------------------------------------------- }}}
------------------------------------------------------------ Workspaces {{{

-- Tags for some "special" workspaces.  These are used by the manage and
-- layout hooks below.
wkC, wkW, wkD, wkM, wkP :: String
wkC = "c"   -- "communication"
wkW = "w"   -- "web"
wkD = "d"   -- "documents"
wkM = "m"   -- "media"
wkP = "p"   -- "presentation"

-- These lists get used inside Keyboard handling and Main.
privworkspaces,deflworkspaces :: [String]
privworkspaces = [wkC,wkW,wkD,wkM,wkP]
deflworkspaces = map show [(1::Int)..9]
addlworkspaces = map (("A" ++) . show) [(1::Int)..9]

-- | Find an empty "default" or "additional" workspace.
findEmptyNumWorkspace :: S.StackSet String l a s sd
                      -> Maybe (S.Workspace String l a)
findEmptyNumWorkspace = find (isNothing . S.stack)
                      . filter (flip elem (deflworkspaces
                                        ++ addlworkspaces) . S.tag)
                      . S.workspaces

----------------------------------------------------------------------- }}}
------------------------------------------------------- Management hook {{{

-- isKDEOverride = do
--    isover <- HMH.isInProperty "_NET_WM_WINDOW_TYPE" "_KDE_NET_WM_WINDOW_TYPE_OVERRIDE"
--    isfs <- HMH.isFullscreen
--    return $! isover && (not isfs)

myManageHook :: ManageHook
myManageHook = composeAll . concat $
    [ [ className   =? c           --> doFloat | c <- myClassFloats]
    , [ title       =? t           --> doFloat | t <- myTitleFloats]
    , [ className   =? "Iceweasel" --> doF (S.shift wkW) ]
    , [ className   =? "Chromium"  --> doF (S.shift wkW) ]
    , [ okularWin                  --> doF (S.shift wkD) ]
    , [ okularPresent              --> doF (S.shift wkP) ]
    , [ HMH.composeOne [ HMH.isFullscreen HMH.-?> HMH.doFullFloat ] ]
    -- , [ HMH.composeOne [ isKDEOverride HMH.-?> doFloat ] ]
    ]
 where
    -- This grabs only Okular root windows, not any dialogs they throw
    -- up.  Since I occasionally move Okulars to other workspaces, this
    -- is handy.
   okularWin     = UW.propertyToQuery $
     (UW.ClassName "Okular") `UW.And` (UW.Role "okular::Shell")
   okularPresent = UW.propertyToQuery $
     (UW.ClassName "Okular") `UW.And` (UW.Role "presentationWidget")
   -- icedlq        = UW.propertyToQuery $ UW.Role "Manager"
   myClassFloats = ["XVkbd", "Xmessage"]
   myTitleFloats = ["KCharSelect"]

----------------------------------------------------------------------- }}}
--------------------------------------------- Action.Eval configuration {{{

myEvalConfig :: AE.EvalConfig
myEvalConfig = AE.defaultEvalConfig
             { AE.imports = [("Prelude",Nothing)
                            ,("System.IO",Nothing)
                            ,("XMonad",Nothing)
                            ,("XMonad.Core",Nothing)
                            ,("XMonad.Util.ExtensibleState", Just "UE")
                            -- ,("System.Posix.IO",Nothing)
                            -- ,("System.Posix.Types",Nothing)
                            ]
             , AE.handleError = \err ->
                 return $ "Error: " ++ (show err)
             }
{-
  where
   self = $(do
             cwd <- TH.runIO $ getWorkingDirectory
             lfn <- liftM TH.loc_filename TH.location
             return . TH.LitE . TH.StringL $ cwd </> lfn)
-}

evalprompt :: X ()
evalprompt = do
    a <- asks (messageHook.config) 
    -- xmd <- getXMonadDir
    PE.evalPromptWithOutput myEvalConfig
        P.amberXPConfig $
       \r -> when (not $ r `elem` ["()",""]) (a $ replace r "\\n" "\n")

----------------------------------------------------------------------- }}}
----------------------------------------------------- Keyboard handling {{{

type Key = (KeyMask, KeySym)

delKeys :: XConfig l -> [Key]
delKeys (XConfig {modMask = modm}) =
    [
        -- mod-q %! Remove this; I almost invariably end up hitting it by
        -- mistake and doing my xmonad restarts from the CLI
        (modm, xK_q)
    ]

addKeys :: XConfig l -> [(Key, X ())]
addKeys (XConfig {modMask = modm}) =
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
    , ((modm, xK_b), smhmdts)
        -- mod-shift-c %! Use CW.kill1 by default.
    , ((modm .|. shiftMask, xK_c     ), CW.kill1)
        -- mod-B %! Toggle xmobar
    , ((modm .|. shiftMask, xK_b ), AXB.togglemyxmobar )
        -- mod-f %! Pull up Bring menu
    , ((modm, xK_f    ), PW.windowPromptBring P.amberXPConfig)
        -- mod-g %! Pull up Goto menu
    , ((modm, xK_g    ), PW.windowPromptGoto  P.amberXPConfig)
        -- mod-G %! Pull up Goto menu filtered for active workspace
    , ((modm .|. shiftMask, xK_g    ), PW.windowPromptGotoCurrent  P.amberXPConfig)
        -- mod-T %! Sink everything on the current desktop
    , ((modm .|. shiftMask, xK_t), AWA.sinkAll)
        -- XF86ScreenSaver %! Lock the screen
        -- mod-x %! Lock the screen
    , ((0, 0x1008ff2d ), xsl)
    , ((modm, xK_x    ), xsl)
        -- for ResizableTall layouts
    , ((modm .|. shiftMask, xK_l ), sendMessage LRT.MirrorShrink)
    , ((modm .|. shiftMask, xK_h ), sendMessage LRT.MirrorExpand)
        -- mod-v %! haskell prompt
    , ((modm, xK_v ), evalprompt )
        -- mod-z %! some utility commands squirreled away behind a submap
    , ((modm, xK_z ), AS.submap . M.fromList $
        [ ((0, xK_b), spawn "blueman-manager")
        , ((0, xK_c), spawn "kcharselect")
        , ((0, xK_d), togglevga)
        , ((0, xK_f), spawn "firefox --no-remote -P default")
        , ((shiftMask, xK_f), spawn "firefox --no-remote -P Flash")
        , ((0, xK_g), spawn "chromium")
        , ((0, xK_r), spawn "gmrun")
        , ((0, xK_w), asks (terminal . config) >>= \t -> spawn $ t ++ " -e wicd-curses")
        ])
        -- mod-\ %! Switch to an unused numeric workspace, or "9" if none.
    , ((modm, xK_backslash), windows $ \ss -> flip S.greedyView ss $
                                 maybe ("9") S.tag
                                     $ findEmptyNumWorkspace ss)
        -- mod-`
    -- , ((modm, xK_quoteleft), return ())
        -- mod-{F1-F12,1-9}
    ] ++ [((modm .|. m .|. m', k), windows $ f i)
          | (i, (k, m')) <-
                zip privworkspaces (map (,0)        [xK_F1..xK_F12])
             ++ zip deflworkspaces (map (,0)        [xK_1 ..xK_9  ])
             ++ zip addlworkspaces (map (,mod1Mask) [xK_1 ..xK_9  ])
          , (f, m) <- [(S.greedyView, 0), (S.shift, shiftMask)
                      ,(CW.copy, shiftMask .|. controlMask)]
                      ]
  where
   xsl = spawn "xscreensaver-command -lock"
   smhmdts = sendMessage HMD.ToggleStruts

   togglevga = do
     screencount <- LIS.countScreens
     if screencount > 1
      then spawn "xrandr --output VGA1 --off"
      else spawn "xrandr --output VGA1 --auto --right-of LVDS1"

----------------------------------------------------------------------- }}}
----------------------------------------------------------- Layout Hook {{{

myLayoutHook =
    LLH.layoutHintsWithPlacement (0.5, 0.5)   -- everybody obeys hinting
  . HMD.avoidStruts                           -- everybody avoids struts
  $ LPW.onWorkspace wkP L.Full                -- presentations always full
  $ LPW.onWorkspaces [wkW, wkD] defaultFull   -- web and docs default full
  $ defaultResizeTall                         -- else, default tall
 where
  defaultResizeTall = lrt ||| lmt ||| L.Full ||| wsslslrt
  defaultFull = L.Full ||| lrt ||| lmt

  lmt = L.Mirror (L.Tall 1 (3/100) (1/2))
  wsslslrt = LS.mksls 1600 lrt lmc
  lmc = LMC.multiCol [1] 2 0.01 (-0.33)
  lrt = LRT.ResizableTall 1 (3/100) (1/2) []


----------------------------------------------------------------------- }}}
------------------------------------------------------------------ Main {{{

main :: IO ()
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
  xmonad $ HUH.withUrgencyHook HUH.NoUrgencyHook
         $ customKeys defaultConfig
      { modMask = mod4Mask
      , terminal = "urxvtcd"
      , workspaces = privworkspaces ++ deflworkspaces ++ addlworkspaces
      , shutdownHook = do
            -- io $ signalProcess keyboardSignal trayp
            AXB.killxmobars
            return ()
      , startupHook = do
            UC.setDefaultCursor UC.xC_left_ptr
            AXB.ensureanxmobar (S 0) "$HOME/lib/X/xmobarrc"
            AXB.updatexmobars
      , manageHook = manageHook defaultConfig <+> HMD.manageDocks <+> myManageHook
      , logHook = AXB.xmobarLH
      , layoutHook = myLayoutHook
      , handleEventHook = AXB.xmobarEH
        -- liftM2 mappend AXB.xmobarEH (return . const (All True))
      }
 where
    customKeys = (EZC.additionalKeys `ap` addKeys)
             . (EZC.removeKeys `ap` delKeys)

{-
main = xmonad defaultConfig
    { startupHook = AE.evalExpressionWithReturn
                        (myEvalConfig {AE.modules = ["xmonad.hs"]})
                        "broadcastMessage (LS.SS (Just \"1\") Nothing)"
                    >>= io . putStrLn . show
    , layoutHook = mksls "foo" Nothing L.Full L.Full
    }
-}

----------------------------------------------------------------------- }}}

-- TODO
-- Urgency hooks

-- VIM modeline, huzzah
-- vim: tw=76 ts=4 expandtab nu ai foldmethod=marker
