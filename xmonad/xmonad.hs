--------------------------------------------------------------- Headers {{{

{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DoRec #-}

import           XMonad
import qualified XMonad.Core as C
import qualified XMonad.Actions.CopyWindow as CW
import qualified XMonad.Actions.CycleWS as CWS
import qualified XMonad.Actions.DynamicWorkspaces as ADW
import qualified XMonad.Actions.Eval as AE
import qualified XMonad.Actions.Warp as AW
import qualified XMonad.Actions.WithAll as AWA
import qualified XMonad.Hooks.DynamicLog as HDL
import qualified XMonad.Hooks.ManageDocks as HMD
import qualified XMonad.Hooks.ManageHelpers as HMH
import qualified XMonad.Layout as L
import qualified XMonad.Layout.IndependentScreens as LIS
import qualified XMonad.Layout.LayoutHints as LLH
import qualified XMonad.Layout.MultiColumns as LMC
import qualified XMonad.Layout.PerWorkspace as LPW
import qualified XMonad.Layout.ResizableTile as LRT
import qualified XMonad.Layout.SLS as LS 
import qualified XMonad.Prompt as P
import qualified XMonad.Prompt.Eval as PE
import qualified XMonad.Prompt.Input as PI
import qualified XMonad.Prompt.Window as PW
import qualified XMonad.StackSet as S
import qualified XMonad.Util.Cursor as UC
import qualified XMonad.Util.ExtensibleState as UE
import qualified XMonad.Util.EZConfig as EZC
import qualified XMonad.Util.Run as UR
import qualified XMonad.Util.WindowProperties as UW

import Control.Concurrent (forkOS)
import Control.Concurrent.MVar
import Control.Exception (try, SomeException)
import Control.Monad (ap,liftM,when)
import Control.Monad.Fix (fix)
import Data.Bits ((.|.))
import qualified Data.IntMap as IM
import Data.Maybe (isNothing)
import Data.List (find, isPrefixOf, stripPrefix)
import Data.Monoid (All(All))
import Data.Ratio ((%))
import System.IO (Handle, hClose, hPutStr, hPutStrLn, stderr)
import System.Posix.Process (executeFile, getProcessStatus, ProcessStatus(..), getProcessID)
import System.Posix.Signals (signalProcess, keyboardSignal)
import System.Posix.Types (ProcessID)

import Graphics.X11.Xinerama (getScreenInfo)

type Key = (KeyMask, KeySym)

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

----------------------------------------------------------------------- }}}
----------------------------------------------------- XMobar management {{{

-- XXX This is not ideal; there should be a pure map of the user's intent
-- and an impure map of the state of the child processes (and the user's
-- intent).  That would let us persist the user's intent across restart.

data XMobars = XMobars (IM.IntMap (MVar (Maybe (Handle, ProcessID))))
    deriving Typeable
instance ExtensionClass XMobars where initialValue = XMobars IM.empty

spawnxmobar ix vv = do
   -- io $ hPutStrLn stderr $ "spawnanxmobar " ++ (show ix)
   v <- liftM Just . UR.spawnPipePid $
                    "exec xmobar -x " ++ (show ix) ++ " " ++ "$HOME/lib/X/xmobarrc"
   () <- io $ putMVar vv v
   case v of
      Nothing -> return () -- no point in waiting
      Just (_,pid) -> waitxmobardeath vv pid >> return ()
   return (ix, vv)
 where
  waitxmobardeath mv pid = io $ forkOS $ fix $ \f -> do
    r <- try $ getProcessStatus True False pid
    -- io $ hPutStrLn stderr $ "waitxmobardeth " ++ (show ix) ++ " (" ++ (show pid) ++ ")"
    case r of
        Left (_ :: SomeException)   -> done
        Right (Nothing)             -> done
        Right (Just (Exited _))     -> done
        Right (Just (Terminated _)) -> done
        Right (Just (Stopped _))    -> f
   where
     done = do
       o <- takeMVar mv
       case o of
         Just (_,p') | p' == pid -> putMVar mv Nothing
         _  -> putMVar mv o

killxmobars_ xs = do
  io $ mapM_ (\x -> takeMVar x
                    >>= maybe (return ()) (killpid . snd)
                    >> putMVar x Nothing)
             xs

respawnxmobars :: X ()
respawnxmobars = do
  screencount <- LIS.countScreens
  XMobars cxmbars <- UE.get
  let (stay, mkill, kills) = IM.splitLookup screencount cxmbars
  -- io $ hPutStrLn stderr $ "respawnxmobars " ++ (show screencount) ++ " " ++ (show $ IM.keys stay)
  killxmobars_ $ maybe (id) (:) mkill (IM.elems kills)
  new <- if (IM.size stay /= screencount)
   then do spawned <- mapM (\ix -> io $ newEmptyMVar >>= spawnxmobar ix)
                [(IM.size stay)..(screencount-1)]
           return $ IM.union stay (IM.fromList spawned)
   else return stay
  UE.put $ XMobars new
  -- XMobars cxmbars <- UE.get
  -- io $ hPutStrLn stderr $ "respawnxmobars end " ++ (show screencount) ++ " " ++ (show $ IM.keys cxmbars)

killxmobars :: X ()
killxmobars = do
  (XMobars old) <- UE.get
  killxmobars_ (IM.elems old)

toggleanxmobar :: ScreenId -> X ()
toggleanxmobar (S scr) = do
  -- io $ hPutStrLn stderr $ "toggleanxmobar " ++ (show scr)
  XMobars cxmbars <- UE.get
  case IM.lookup scr cxmbars of
    Just mxv -> do
        mx <- io $ takeMVar mxv
        case mx of
            Nothing -> spawnxmobar scr mxv >> return ()
            Just (_,p) -> killpid p >> io (putMVar mxv Nothing)
    Nothing -> return ()

togglemyxmobar :: X ()
togglemyxmobar = do
  cws <- gets windowset
  toggleanxmobar (S.screen $ S.current $ cws)

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

-- These lists gets used inside Keyboard handling and Main.
privworkspaces = [wkC,wkW,wkD,wkM,wkP]
deflworkspaces = map show [1..9]

-- | Find an empty "default" ("numeric", tag "1" to "9") workspace.
findEmptyNumWorkspace :: S.StackSet String l a s sd -> Maybe (S.Workspace String l a)
findEmptyNumWorkspace = find (isNothing . S.stack)
                      . filter (flip elem (deflworkspaces) . S.tag)
                      . S.workspaces

----------------------------------------------------------------------- }}}
------------------------------------------------------- Management hook {{{

-- isKDEOverride = do
--    isover <- HMH.isInProperty "_NET_WM_WINDOW_TYPE" "_KDE_NET_WM_WINDOW_TYPE_OVERRIDE"
--    isfs <- HMH.isFullscreen
--    return $! isover && (not isfs)

myManageHook = composeAll . concat $
    [ [ className   =? c           --> doFloat | c <- myClassFloats]
    , [ title       =? t           --> doFloat | t <- myTitleFloats]
    , [ className   =? "Iceweasel" --> doF (S.shift wkW) ]
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
------------------------------------------------------------ Event hook {{{

myEventHook :: Event -> X All
myEventHook (ConfigureEvent {ev_window = w}) = do
    whenX (isRoot w) respawnxmobars
    return (All True)
myEventHook _ = return (All True)

----------------------------------------------------------------------- }}}
--------------------------------------------- Action.Eval configuration {{{

myEvalConfig :: AE.EvalConfig
myEvalConfig = AE.defaultEvalConfig {AE.imports = [("Prelude",Nothing)
                                                  ,("Data.Map",Just "M")
                                                  ,("System.IO",Nothing)
                                                  -- ,("System.Posix.IO",Nothing)
                                                  -- ,("System.Posix.Types",Nothing)
                                                  ,("XMonad",Nothing)
                                                  ,("XMonad.Core",Nothing)
                                                  ,("XMonad.StackSet",Just "S")
                                                  ,("XMonad.Util.ExtensibleState",Just "UE")
                                                  ]
                                    ,AE.handleError = \err ->
                                        return $ "Error: " ++ replace (show err) "\\n" "\n"
                                    }

evalprompt = do
    a <- asks (messageHook.config) 
    PE.evalPromptWithOutput myEvalConfig P.defaultXPConfig $
        \r -> when (not $ r `elem` ["()",""]) (a r)

----------------------------------------------------------------------- }}}
----------------------------------------------------- Keyboard handling {{{

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
    , ((modm, xK_b), smhmdts)
        -- mod-shift-c %! Use CW.kill1 by default.
    , ((modm .|. shiftMask, xK_c     ), CW.kill1)
        -- mod-B %! Toggle xmobar
    , ((modm .|. shiftMask, xK_b ), togglemyxmobar )
        -- mod-f %! Pull up Bring menu
    , ((modm, xK_f    ), PW.windowPromptBring P.amberXPConfig)
        -- mod-g %! Pull up Goto menu
    , ((modm, xK_g    ), PW.windowPromptGoto  P.amberXPConfig)
        -- mod-G %! Pull up Goto menu filtered for active workspace
    , ((modm .|. shiftMask, xK_g    ), PW.windowPromptGotoCurrent  P.amberXPConfig)
        -- mod-T %! Sink everything on the current desktop
    , ((modm .|. shiftMask, xK_t), AWA.sinkAll)
        -- mod-o %! Pull up chraracter selector
    , ((modm .|. shiftMask, xK_o    ), spawn "kcharselect")
        -- XF86ScreenSaver %! Lock the screen
        -- mod-x %! Lock the screen
    , ((0, 0x1008ff2d ), xsl)
    , ((modm, xK_x    ), xsl)
        -- for ResizableTall layouts
    , ((modm .|. shiftMask, xK_l ), sendMessage LRT.MirrorShrink)
    , ((modm .|. shiftMask, xK_h ), sendMessage LRT.MirrorExpand)
        -- mod-v %! haskell prompt
    , ((modm, xK_v ), evalprompt )
        -- mod-\ %! Switch to an unused numeric workspace, or "9" if none.
    , ((modm, xK_backslash), windows $ \ss -> flip S.greedyView ss $
                                 maybe ("9") S.tag
                                     $ findEmptyNumWorkspace ss)
        -- mod-{F1-F12,1-9}
    ] ++ [((modm .|. m, k), windows $ f i)
          | (i, k) <-    zip privworkspaces [xK_F1..xK_F12]
                      ++ zip deflworkspaces [xK_1 ..xK_9  ]
          , (f, m) <- [(S.greedyView, 0), (S.shift, shiftMask), (CW.copy, shiftMask .|. controlMask)]]
  where
   xsl = spawn "xscreensaver-command -lock"
   smhmdts = sendMessage HMD.ToggleStruts

----------------------------------------------------------------------- }}}
----------------------------------------------------------- Layout Hook {{{

myLayoutHook =
    HMD.avoidStruts                           -- everybody avoids struts
  . LLH.layoutHintsWithPlacement (0.5, 0.5)   -- and obeys hinting
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
      , workspaces = privworkspaces ++ deflworkspaces
      , shutdownHook = do
            -- io $ signalProcess keyboardSignal trayp
            killxmobars
      , startupHook = do
            UC.setDefaultCursor UC.xC_left_ptr
            respawnxmobars
      , manageHook = manageHook defaultConfig <+> HMD.manageDocks <+> myManageHook
      , logHook = do
           XMobars cxmbars <- UE.get
           HDL.dynamicLogWithPP $ HDL.xmobarPP
                   { HDL.ppOutput = \o ->
                       mapM_ (\x -> readMVar x >>= maybe (return ())
                                    (\(h,_) -> hPutStrLn h o))
                             (IM.elems cxmbars)
                   , HDL.ppTitle = HDL.xmobarColor "green" "" . HDL.shorten 40
                   , HDL.ppLayout = \s -> maybe s id $ stripPrefix "Hinted " s
                   }
      , layoutHook = myLayoutHook
      , handleEventHook = myEventHook
      }
 where
    customKeys = (EZC.additionalKeys `ap` addKeys)
             . (EZC.removeKeys `ap` delKeys)

----------------------------------------------------------------------- }}}

-- TODO
-- XMobar fixes (pure maps, multiple configurations)
-- Urgency hooks

-- VIM modeline, huzzah
-- vim: tw=76 ts=4 expandtab nu ai foldmethod=marker
