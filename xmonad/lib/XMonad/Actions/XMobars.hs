--------------------------------------------------------------- Headers {{{
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module XMonad.Actions.XMobars (
         ensureanxmobar,
         togglemyxmobar,
         updatexmobars,
         killxmobars,
         xmobarEH,
         xmobarLH,
) where

import           XMonad
import qualified XMonad.Hooks.DynamicLog as HDL
import qualified XMonad.Layout.IndependentScreens as LIS
import qualified XMonad.StackSet as S
import qualified XMonad.Util.ExtensibleState as UE
import qualified XMonad.Util.Run as UR
import qualified XMonad.Util.WindowProperties as UW

import Control.Exception (SomeException, handle)
import Control.Monad (foldM,when)
import qualified Data.IntMap as IM
import Data.List (find, stripPrefix)
import qualified Data.Map as M
import Data.Maybe (fromJust)
import Data.Monoid (All(..))
import System.IO (Handle, hClose, hPutStrLn) -- stderr
import System.Posix.Signals (signalProcess, keyboardSignal)
import System.Posix.Types (ProcessID)

----------------------------------------------------------------------- }}}
----------------------------------------------------------------- Types {{{

data XMobars = XMobars {
        -- | A temporary holding map for xmobars by PID until
        -- we see them report in at the event hook.
      xmPidScreen :: M.Map ProcessID ScreenId
        -- The long-term storage locations:
    , xmScreenState :: IM.IntMap (Handle, ProcessID)
    , xmScreenIntent :: IM.IntMap (Maybe String)
    , xmWinScreen :: M.Map Window ScreenId
    , xmScreenWin :: IM.IntMap Window
        -- Configuration variables
    -- , xmProgName :: String
    -- , xmDefaultConfig :: String
    }
  deriving (Typeable)
instance ExtensionClass XMobars where
    initialValue = XMobars M.empty IM.empty IM.empty M.empty IM.empty
    extensionType = PersistentExtension
instance Show XMobars where
    show x = "XMobars (" ++ (show $ xmScreenIntent x) ++ ")"
instance Read XMobars where
    readsPrec _ r = [ (initialValue {xmScreenIntent = xmsi}, s) |
                      ("XMobars",r') <- lex r,
                      (xmsi, s) <- readsPrec 11 r'
                    ]

----------------------------------------------------------------------- }}}
----------------------------------------------------------------- Funcs {{{

killxmobar :: Bool -> XMobars -> ScreenId -> IO XMobars
killxmobar dokill xmbs (S ix) = do
  -- io $ hPutStrLn stderr $ "killxmobar " ++ (show dokill) ++ " " ++ (show ix)
  case IM.lookup ix (xmScreenState xmbs) of
    Nothing -> return xmbs
    Just (h, pid) -> do
      when dokill $ killpid pid
      ignoreExn (hClose h)
      return $ xmbs
        { xmPidScreen =    M.delete pid (xmPidScreen   xmbs)
        , xmScreenState = IM.delete ix  (xmScreenState xmbs)
        , xmScreenWin =   IM.delete ix  (xmScreenWin   xmbs)
        , xmScreenIntent = IM.delete ix (xmScreenIntent xmbs)
        , xmWinScreen =   maybe (xmWinScreen xmbs)
                                (\w -> M.delete w (xmWinScreen xmbs))
                                (IM.lookup ix (xmScreenWin xmbs))
        }

updatexmobar :: XMobars -> ScreenId -> IO XMobars
updatexmobar xmbs (S ix) = do
   let mcfg = IM.lookup ix (xmScreenIntent xmbs)
   -- io $ hPutStrLn stderr $ "updatexmobar " ++ (show ix) ++ " " ++ (show mcfg)
   case mcfg of
     Just (Just cfg) -> case IM.lookup ix (xmScreenState xmbs) of
       Nothing -> do
         r@(_, pid) <- UR.spawnPipePid $
                       "exec /home/nwf/bin/xmobar -x " ++ (show ix) 
                         ++ " " ++ cfg
         let nps =  M.insert pid (S ix) (xmPidScreen   xmbs)
         let ns  = IM.insert ix  r      (xmScreenState xmbs)
         return $ xmbs { xmPidScreen = nps, xmScreenState = ns }
       Just _ -> return xmbs
     _ -> killxmobar True xmbs (S ix)

updatexmobars_ :: XMobars -> X XMobars
updatexmobars_ xmbs = do
  screencount <- LIS.countScreens
  -- io $ hPutStrLn stderr $ "updatexmobar " ++ (show screencount) ++ ":" ++ (show xmbs)
  let (_, mkill, okill) = IM.splitLookup screencount (xmScreenState xmbs)
  let kills = maybe (id) (:) mkill (IM.elems okill)

  -- io $ hPutStrLn stderr $ "updatexmobar killing " ++ (show kills)

  mapM_ (\(h,p) -> io $ killpid p >> ignoreExn (hClose h)) kills
  let sk = map snd kills
  let nps = foldl (flip M.delete) (xmPidScreen xmbs) sk
  let ws = map (flip IM.lookup (xmScreenWin xmbs) . fromIntegral) sk
  let nsw = foldl (\a b -> IM.delete (fromIntegral b) a) (xmScreenWin xmbs) sk
  let nws = foldl (\a b -> case b of
                           Nothing -> a
                           Just x' -> M.delete x' a) (xmWinScreen xmbs) ws
  foldM (\a b -> io $ updatexmobar a (S b))
        (xmbs { xmPidScreen = nps
              , xmWinScreen = nws
              , xmScreenWin = nsw})
        (IM.keys $ fst . IM.split screencount $ xmScreenIntent xmbs)

updatexmobars :: X ()
updatexmobars = UE.put =<< updatexmobars_ =<< UE.get

ensureanxmobar_ :: ScreenId -> String -> XMobars -> XMobars
ensureanxmobar_ (S s) c xmbs =
  case IM.lookup s (xmScreenIntent xmbs) of
    Just (Just _) -> xmbs
    _             -> xmbs { xmScreenIntent = IM.insert s (Just c)
                                                (xmScreenIntent xmbs) }

ensureanxmobar :: ScreenId -> String -> X ()
ensureanxmobar s c = UE.put =<< return . ensureanxmobar_ s c =<< UE.get

toggleanxmobar :: ScreenId -> X ()
toggleanxmobar (S scr) = do
  -- io $ hPutStrLn stderr $ "toggleanxmobar " ++ (show scr)
  xmbs <- UE.get
  let nmcfg = case IM.lookup scr (xmScreenIntent xmbs) of
                Just (Just _) -> Nothing
                _ -> Just "$HOME/lib/X/xmobarrc"    -- XXX
  let xmbs' = xmbs {xmScreenIntent = IM.insert scr nmcfg (xmScreenIntent xmbs)}
  UE.put =<< (liftIO $ updatexmobar xmbs' (S scr))

togglemyxmobar :: X ()
togglemyxmobar = do
  cws <- gets windowset
  toggleanxmobar (S.screen $ S.current $ cws)

killxmobars :: X ()
killxmobars = do
  xmbs <- UE.get
  UE.put =<< foldM (\xs' s -> io $ killxmobar True xs' (S s)) xmbs
                   (IM.keys $ xmScreenWin xmbs)

----------------------------------------------------------------------- }}}
-------------------------------------------------------------- Log hook {{{

xmobarLH :: X ()
xmobarLH = do
     ws <- gets windowset
     (xmScreenState <$> UE.get)
      >>= IM.foldWithKey (\s (h,_) a -> a >>
            HDL.dynamicLogWithPP (base
            { HDL.ppOutput = hPutStrLn h
            , HDL.ppTitle = HDL.xmobarColor "goldenrod" ""
            , HDL.ppTitleSanitize = xmoRaw . HDL.shorten 40
            , HDL.ppLayout = \n -> maybe n id $ stripPrefix "Hinted " n
            , HDL.ppCurrent = HDL.xmobarColor "red" "" .
                 if (S s) /= S.screen (S.current ws)
                  then HDL.wrap "{" "}"
                  else HDL.wrap "[" "]"
            , HDL.ppVisible = \w ->
                      -- fromJust safe here since ppVisible called only for
                      -- `elem` (map (S.tag . S.workspace) (S.visible ws))
                  let scr = fromJust $ find ((== w) . S.tag . S.workspace)
                                            (S.visible ws)
                  in if (S s) == S.screen scr
                      then HDL.wrap "<" ">" w
                      else HDL.ppVisible base w
            , HDL.ppUrgent = HDL.xmobarColor "darkgoldenrod" "green" 
            }))
        (return ()) . (xmScreenState)
 where base = HDL.xmobarPP
       xmoRaw x = "<raw=" ++ (show (length x)) ++ ":" ++ x ++ "/>"

----------------------------------------------------------------------- }}}
------------------------------------------------------------ Event hook {{{
-- xmobarEH :: EventHook
xmobarEH (ConfigureEvent {ev_window = w}) = do
  whenX (isRoot w) updatexmobars
{-
      rs <- withDisplay (io . getScreenInfo)
      let (_,ix,wid) = foldl' (\(ctr,bix,bwid) (Rectangle _ _ w _) ->
                                if w >= bwid
                                 then (ctr+1,ctr,w)
                                 else (ctr+1,bix,bwid))
                              (0,0,0) rs
      ...
-}
  return$All True
xmobarEH (MapNotifyEvent {ev_window = w}) = do
    xmbs <- UE.get
    when (not $ M.null $ xmPidScreen xmbs) $ do
      pids <- UW.getProp32s "_NET_WM_PID" w
      case pids of
        Nothing -> return ()
        Just [] -> return ()
        Just (pid:_) -> do
          case M.lookup (fromIntegral pid) $ xmPidScreen xmbs of
            Nothing -> return ()
            Just (S scr) -> do
              -- io $ hPutStrLn stderr $ "XMB: " ++ (show pids) ++ "@" ++ (show w)
              let ws' = M.insert w (S scr) $ xmWinScreen xmbs
              let sw' = IM.insert scr w $ xmScreenWin xmbs
              let ps' = M.delete (fromIntegral pid) $ xmPidScreen xmbs
              UE.put $ xmbs { xmPidScreen = ps'
                            , xmWinScreen = ws'
                            , xmScreenWin = sw'
                            }
    return$All True
xmobarEH (DestroyWindowEvent {ev_window = w}) = do
    xmbs <- UE.get
    case M.lookup w (xmWinScreen xmbs) of
      Nothing -> return ()
      Just scr -> do
        -- io $ hPutStrLn stderr $ "xmobar death: " ++ (show scr)
        liftIO (killxmobar False xmbs scr) >>= UE.put
    return$All True
xmobarEH _ = return$All True
----------------------------------------------------------------------- }}}
----------------------------------------------------------------- Utils {{{

ignoreExn :: IO () -> IO ()
ignoreExn = handle (\(_ :: SomeException) -> return ())

killpid :: MonadIO m => ProcessID -> m ()
killpid = io . ignoreExn . signalProcess keyboardSignal
----------------------------------------------------------------------- }}}
-- vim: tw=76 ts=4 expandtab nu ai foldmethod=marker
