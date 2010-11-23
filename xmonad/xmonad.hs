--------------------------------------------------------------- Headers {{{

{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DoRec #-}

import           XMonad
import qualified XMonad.Core as C
import qualified XMonad.Actions.CycleWS as CWS
import qualified XMonad.Actions.DynamicWorkspaces as ADW
import qualified XMonad.Actions.Eval as AE
import qualified XMonad.Actions.Warp as AW
import qualified XMonad.Hooks.DynamicLog as HDL
import qualified XMonad.Hooks.ManageDocks as HMD
import qualified XMonad.Hooks.ManageHelpers as HMH
import qualified XMonad.Layout as L
import qualified XMonad.Layout.IndependentScreens as LIS
import qualified XMonad.Layout.LayoutHints as LLH
import qualified XMonad.Layout.ResizableTile as LRT
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
import Data.List (isPrefixOf)
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
------------------------------------------------------ Named Workspaces {{{

-- This list gets used inside Keyboard handling and Main, below.
-- The names on it are used inside the management hook.
--
namedwksptags = [ ("c", xK_F1)   -- "communication"
                , ("w", xK_F2)   -- "web"
                , ("d", xK_F3)   -- "documents"
                , ("m", xK_F4)   -- "media"
                , ("p", xK_F5)   -- "presentation"
                ]

----------------------------------------------------------------------- }}}
------------------------------------------------------- Management hook {{{

-- isKDEOverride = do
--    isover <- HMH.isInProperty "_NET_WM_WINDOW_TYPE" "_KDE_NET_WM_WINDOW_TYPE_OVERRIDE"
--    isfs <- HMH.isFullscreen
--    return $! isover && (not isfs)

myManageHook = composeAll . concat $
    [ [ className   =? c --> doFloat           | c <- myClassFloats]
    , [ title       =? t --> doFloat           | t <- myTitleFloats]
    , [ className   =? "Iceweasel" --> doF (S.shift "w") ]
    , [ okularWin        --> doF (S.shift "d") ]
    , [ okularPresent    --> doF (S.shift "p") ]
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
        -- mod-f %! Pull up Bring menu
    , ((modm, xK_f    ), PW.windowPromptBring P.defaultXPConfig)
        -- mod-g %! Pull up Goto menu
    , ((modm, xK_g    ), PW.windowPromptGoto  P.defaultXPConfig)
        -- mod-G %! Pull up Goto menu filtered for active workspace
    , ((modm .|. shiftMask, xK_g    ), PW.windowPromptGotoCurrent  P.defaultXPConfig)
        -- mod-o %! Pull up chraracter selector
    , ((modm .|. shiftMask, xK_o    ), spawn "kcharselect")
        -- XF86ScreenSaver or XF86PowerOff lock the screen
    , ((0, 0x1008ff2d ), xsl)
    , ((0, 0x1008ff2a ), xsl)
        -- for ResizableTall layouts
    , ((modm .|. shiftMask, xK_l ), sendMessage LRT.MirrorShrink)
    , ((modm .|. shiftMask, xK_h ), sendMessage LRT.MirrorExpand)
        -- mod-b %! Toggle Struts
    , ((modm, xK_b), smhmdts)
        -- mod-B %! Toggle xmobar
    , ((modm .|. shiftMask, xK_b ), togglemyxmobar )
        -- mod-v %! haskell prompt
    , ((modm, xK_v ), evalprompt )
    ] ++ [((modm .|. m, k), windows $ f i)
          | (i, k) <- namedwksptags
          , (f, m) <- [(S.greedyView, 0), (S.shift, shiftMask)]]
  where
   xsl = spawn "xscreensaver-command -lock"
   smhmdts = sendMessage HMD.ToggleStruts

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
      , workspaces = workspaces defaultConfig ++ map fst namedwksptags
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
                   , HDL.ppTitle = HDL.xmobarColor "green" "" . HDL.shorten 20
                   }
      , layoutHook = HMD.avoidStruts . LLH.layoutHintsWithPlacement (0.5, 0.5)
                           $ LRT.ResizableTall 1 (3/100) (1/2) []
                         ||| L.Mirror (L.Tall 1 (3/100) (1/2))
                         ||| L.Full
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
