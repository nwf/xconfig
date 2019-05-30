--------------------------------------------------------------- Headers {{{

{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

import           XMonad
-- import qualified XMonad.Core as C
import qualified XMonad.Actions.CopyWindow as CW
import qualified XMonad.Actions.CycleWS as CWS
import qualified XMonad.Actions.DynamicWorkspaceGroups as ADWG
-- import qualified XMonad.Actions.DynamicWorkspaces as ADW
import qualified XMonad.Actions.Submap as AS
import qualified XMonad.Actions.Warp as AW
import qualified XMonad.Actions.WithAll as AWA
import qualified XMonad.Actions.XMobars as AXB
import qualified XMonad.Hooks.ManageDocks as HMD
import qualified XMonad.Hooks.ManageHelpers as HMH
import           XMonad.Hooks.ManageHelpers ( (-?>) )
import qualified XMonad.Hooks.MiniEwmh as HME
import qualified XMonad.Hooks.SetWMName as HSW
import qualified XMonad.Hooks.UrgencyHook as HUH
import qualified XMonad.Layout as L
import qualified XMonad.Layout.IndependentScreens as LIS
import qualified XMonad.Layout.IM as LIM
import qualified XMonad.Layout.LayoutHints as LLH
import qualified XMonad.Layout.MultiColumns as LMC
import qualified XMonad.Layout.PerWorkspace as LPW
import qualified XMonad.Layout.Reflect as LR
import qualified XMonad.Layout.ResizableTile as LRT
import qualified XMonad.Layout.SLS as LS 
import qualified XMonad.Prompt as P
-- import qualified XMonad.Prompt.Input as PI
import qualified XMonad.Prompt.Window as PW
import qualified XMonad.StackSet as S
import qualified XMonad.Util.Cursor as UC
-- import qualified XMonad.Util.ExtensibleState as UE
import qualified XMonad.Util.EZConfig as EZC
import qualified XMonad.Util.WindowProperties as UW
import qualified XMonad.Util.WorkspaceCompare as UWC
import qualified XMonad.Util.XRandRUtils as UXRR

#ifdef BUILD_EVAL
import qualified XMonad.Actions.Eval as AE
import qualified XMonad.Prompt.Eval as PE
#endif


import Control.Applicative ((<$>))
import Control.Monad (ap,when) -- liftM2
import Data.List (find, isPrefixOf) -- stripPrefix
import qualified Data.Map as M
import Data.Maybe (fromJust, isNothing)
import Data.Monoid (All(All), mappend)
import Data.Ratio ((%))
import qualified Data.Set as Set
-- import qualified Language.Haskell.TH as TH
-- import System.FilePath ((</>))
-- import System.IO (hPutStrLn, stderr)
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

{-
-- | Do some operation but re-focus on the screen thereafter.  Useful as a
-- workaround, but somewhat unfortunate.
holdScreenFocus :: X a -> X a
holdScreenFocus a = do
  s <- gets (S.screen . S.current . windowset)
  r <- a
  screenWorkspace s >>= maybe (return ()) (windows . S.view)
  return r
-}

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
privworkspaces,deflworkspaces,addlworkspaces :: [String]
privworkspaces = [wkC,wkW,wkD,wkM,wkP]
deflworkspaces = map show [(1::Int)..9]
addlworkspaces = map (("A" ++) . show) [(1::Int)..9]

-- | Predicate for empty "default" or "additional" workspace.
isEmptyNumWorkspace :: X (WindowSpace -> Bool)
isEmptyNumWorkspace = return $ \ws -> (isNothing $ S.stack ws)
                                   && (S.tag ws `Set.member` searchset)
 where
  searchset = Set.fromList $ deflworkspaces ++ addlworkspaces

----------------------------------------------------------------------- }}}
------------------------------------------------------- Management hook {{{

isKDEOverride = HMH.isInProperty "_NET_WM_WINDOW_TYPE" "_KDE_NET_WM_WINDOW_TYPE_OVERRIDE"

-- | Work around X11 being "mechanism and no policy" by trying to induce a
-- policy.  Of course, if clients had to ask a capability for a new window
-- capability, this would be much easier and we'd not be at the mercy of
-- applications behaving correctly.  Le sigh.
myManageHook :: ManageHook
myManageHook = composeAll $ [shift, float]
     where
   myClassFloats = ["XVkbd", "Xmessage", "MPlayer"]
   myTitleFloats = ["KCharSelect", "wicd-curses"]
   
   shift = HMH.composeOne $
        [ HMH.transience
        , className   =? "Iceweasel" -?> doF (S.shift wkW)
        , className   =? "Chromium"  -?> doF (S.shift wkW)
        , className   =? "Okular"    -?>
            HMH.composeOne $ [
                -- Apparently Okular no longer sets this.  Boo.
                -- UW.propertyToQuery (UW.Role "presentationWidget") -?> doF (S.shift wkP)
                isKDEOverride -?> doF (S.shift wkP)
            ,   Just <$> doF (S.shift wkD)
            ]
        ]

   float = HMH.composeOne $ concat
    [ [ HMH.isDialog               -?> doFloat ]
    , [ HMH.isFullscreen           -?> HMH.doFullFloat ]
    , [ className   =? c           -?> doFloat | c <- myClassFloats ]
    , [ title       =? t           -?> doFloat | t <- myTitleFloats ]
    , [ ("ImageMagick:" `isPrefixOf`) <$> title -?> doFloat ]
    ]


----------------------------------------------------------------------- }}}
------------------ Action.Eval configuration (including static disable) {{{

#ifdef BUILD_EVAL
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
    PE.evalPromptWithOutput myEvalConfig
        P.amberXPConfig $
       \r -> when (not $ r `elem` ["()",""]) (a $ replace r "\\n" "\n")
#endif

----------------------------------------------------------------------- }}}
----------------------------------------------------- Keyboard handling {{{

type Key = (KeyMask, KeySym)

delKeys :: XConfig l -> [Key]
delKeys (XConfig {modMask = modm}) =
    [
        -- mod-q %! Remove this; I almost invariably end up hitting it by
        -- mistake and doing my xmonad restarts from the CLI
        (modm, xK_q)
    ,   (modm .|. shiftMask, xK_slash)
    ]

addKeys :: XConfig l -> [(Key, X ())]
addKeys (XConfig {modMask = modm}) =
    [
        -- mod-` %! Toggle to the workspace displayed previously
      ((modm, xK_grave), CWS.toggleWS)
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
        -- mod-f %! Pull up Bring menu
    , ((modm, xK_f    ), PW.windowPromptBring P.amberXPConfig)
        -- mod-g %! Pull up Goto menu
    , ((modm, xK_g    ), PW.windowPromptGoto  P.amberXPConfig)
        -- mod-G %! Pull up Goto menu filtered for active workspace
    , ((modm .|. shiftMask, xK_g    ), PW.windowPromptGotoCurrent  P.amberXPConfig)
        -- mod-T %! Sink everything on the current desktop
    , ((modm .|. shiftMask, xK_t), AWA.sinkAll)
        -- XF86ScreenSaver %! Lock the screen
        --   (that funny-looking number comes from X11/XF86keysym.h)
        -- mod-x %! Lock the screen
    , ((0, 0x1008ff2d ), xsl)
    , ((modm, xK_x    ), xsl)
        -- for ResizableTall layouts
    , ((modm .|. shiftMask, xK_l ), sendMessage LRT.MirrorShrink)
    , ((modm .|. shiftMask, xK_h ), sendMessage LRT.MirrorExpand)

#ifdef BUILD_EVAL
        -- mod-v %! haskell prompt, if statically enabled (is huge!)
    , ((modm, xK_v ), evalprompt )
#endif

        -- mod-z %! some utility commands squirreled away behind a submap
    , ((modm, xK_z ), AS.submap . M.fromList $
        [ ((0, xK_b), spawn "blueman-manager")
        , ((0, xK_c), spawn "kcharselect")
        , ((0, xK_d), toggledisplay "HDMI1")
        , ((shiftMask, xK_d), toggledisplay "VGA1")
        , ((0, xK_f), spawn "firefox --no-remote -P default")
        , ((shiftMask, xK_f), spawn "firefox --no-remote -P Flash")
        , ((0, xK_g), spawn "chromium")
        , ((0, xK_h), asks (terminal . config) >>= \t -> spawn $ t ++ " -e ghci")
        , ((0, xK_m), AXB.togglemyxmobar)
        , ((0, xK_r), spawn "gmrun")
        , ((0, xK_v), spawn "gvim")
        , ((0, xK_w), asks (terminal . config) >>= \t -> spawn $ t ++ " -g 100x40 -e wicd-curses")
        , ((0, xK_z), asks (terminal . config) >>= \t -> spawn t)
        ])
        -- mod-\ %! Switch to an unused numeric workspace
    , ((modm, xK_backslash), CWS.moveTo CWS.Next (CWS.WSIs isEmptyNumWorkspace))
        -- mod-| %! Move the focused window to an unused workspace and then
        -- focus there.
    , ((modm .|. shiftMask, xK_backslash), mtsc)
        -- mod-s %! Swap contents of current screen with next; focus stays
        -- here.  Note that this is different from CWS.toggleWS, which
        -- switches focus to the next *invisible* workspace.
    , ((modm,               xK_s), CWS.swapNextScreen)
        -- mod-q %! Focus on next screen or move window to next screen
    , ((modm,               xK_q), CWS.nextScreen)
    , ((modm .|. shiftMask, xK_q), CWS.shiftNextScreen)
        -- mod-/ and mod-? %! Jump to or memorize a workspace group
    , ((modm              , xK_slash), ADWG.viewWSGroup       "modslash")
    , ((modm .|. shiftMask, xK_slash), ADWG.addCurrentWSGroup "modslash")
        -- mod-{F1-F12,1-9,alt&1-9}
        --   No additional mods to view,
        --   Shift to move current window to,
        --   Control+Shift to move current window to and follow
    ] ++ [((modm .|. m .|. m', k), windows $ f i)
          | (i, (k, m')) <-
                zip privworkspaces (map (,0)        [xK_F1..xK_F12])
             ++ zip deflworkspaces (map (,0)        [xK_1 ..xK_9  ])
             ++ zip addlworkspaces (map (,mod1Mask) [xK_1 ..xK_9  ])
          , (f, m) <- [(S.greedyView, 0), (S.shift, shiftMask)
                      ,(shiftThenView, shiftMask .|. controlMask)]
                      ]
  where
   xsl = spawn "xscreensaver-command -lock"
   smhmdts = sendMessage HMD.ToggleStruts

   mtsc = CWS.doTo CWS.Next (CWS.WSIs isEmptyNumWorkspace) UWC.getSortByIndex $
          windows . shiftThenView

   shiftThenView wsid = S.greedyView wsid . S.shift wsid

   toggledisplay d = do
     UXRR.getXRRConnStatus d >>= \ case
       Nothing               -> pure ()  -- no such output
       Just (UXRR.XCSOff   ) -> pure ()  -- nothing there to turn on
       Just (UXRR.XCSDiscon) -> off
       Just (UXRR.XCSConn  ) ->
         spawn $ "xrandr --output " ++ d ++ " --auto --right-of LVDS1"
       Just (UXRR.XCSEna   ) -> off
    where
     off = spawn $ "xrandr --output " ++ d ++ " --off"

-- we might consider hooking 0x1008FF2C (eject) which I use as a hog-killer

----------------------------------------------------------------------- }}}
----------------------------------------------------------- Layout Hook {{{

myLayoutHook =
    LLH.layoutHintsWithPlacement (0.5, 0.5)   -- everybody obeys hinting
  . HMD.avoidStruts                           -- everybody avoids struts
  $ LPW.onWorkspace wkP L.Full                -- presentations always full
  $ LPW.onWorkspaces [wkC, wkW, wkD] defaultFull  -- some default full and
                                                  -- have a shorter set
  $ defaultResizeTall                         -- else, default tall
 where
  defaultResizeTall = lrt ||| lmt ||| L.Full ||| gimp ||| wsslslrt
  defaultFull = L.Full ||| lrt ||| lmt

  lmt = L.Mirror (LRT.ResizableTall 1 (3/100) (1/2) [])
  wsslslrt = LS.mksls 1600 lrt lmc
  lmc = LMC.multiCol [1,1] 2 0.01 (-0.33)
  lrt = LRT.ResizableTall 1 (3/100) (1/2) []

        -- From http://nathanhowell.net/2009/03/08/xmonad-and-the-gimp/
  gimp = LIM.withIM (0.11) (UW.Role "gimp-toolbox") $
             LR.reflectHoriz $
             LIM.withIM (0.15) (UW.Role "gimp-dock") L.Full

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
         $ customKeys
         $ def
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
            -- HME.mewmhSH
            HSW.setWMName "xmonad"
      , manageHook = HMD.manageDocks <+> myManageHook
      , logHook = {- HME.mewmhLH >> -} AXB.xmobarLH
      , layoutHook = myLayoutHook
      , handleEventHook =           HMD.docksEventHook
                          `mappend` AXB.xmobarEH
                          -- `mappend` P.eventHandle
                          -- `mappend` leh
      }
 where
    customKeys = (EZC.additionalKeys `ap` addKeys)
             . (EZC.removeKeys `ap` delKeys)
    leh _ e = (io$putStrLn$"LEH " ++ show e) >> (return$All True)

----------------------------------------------------------------------- }}}

-- VIM modeline, huzzah
-- vim: tw=76 ts=4 expandtab nu ai foldmethod=marker
--
-- Open challenge: reset layout when last window closes, using EH
