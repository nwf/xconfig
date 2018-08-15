--------------------------------------------------------------- Headers {{{
{-# LANGUAGE LambdaCase #-}

---------------------------------------------------------------------------
-- |
-- Module       : XMonad.Hooks.Util.XRandRUtils
-- Copyright    : (C) Nathaniel Wesley Filardo <nwfilardo@gmail.com>
-- License      : BSD
--
-- Maintainer   : Nathaniel Wesley Filardo <nwfilardo@gmail.com>
-- Stability    : unstable
-- Portability  : unportable
--

module XMonad.Util.XRandRUtils (
  XRRConnStatus(..), getXRRConnStatus
) where

import           Control.Applicative
import           Graphics.X11.Xlib
import           Graphics.X11.Xrandr

import           Data.Maybe (mapMaybe)

import           XMonad

----------------------------------------------------------------------- }}}
--------------------------------------------- Monitor Connection Status {{{

data XRRConnStatus = XCSOff     -- ^ Disconnected and no CRTC
                   | XCSDiscon  -- ^ Disconnected but asssigned CRTC
                   | XCSConn    -- ^ Connected but no assigned CRTC
                   | XCSEna     -- ^ Connected and assigned CRTC
 deriving (Show)

getXRRConnStatus :: String -> X (Maybe XRRConnStatus)
getXRRConnStatus m = withDisplay (liftIO . flip getXRRConnStatusIO m)

getXRRConnStatusIO :: Display -> String -> IO (Maybe XRRConnStatus)
getXRRConnStatusIO dpy m = do
  mxrs <- rootWindow dpy (defaultScreen dpy) >>= xrrGetScreenResources dpy
  case mxrs of
    Nothing -> pure Nothing
    Just xrs -> do
      axr <- getAllXRROI dpy xrs
      pure $ (snd <$> fmap status <$> findXRROI m axr)

status :: XRROutputInfo -> XRRConnStatus
status (XRROutputInfo {xrr_oi_crtc = c, xrr_oi_modes = m}) =
  case (null m, c) of
    (True, 0) -> XCSOff
    (True, _) -> XCSDiscon
    (False, 0) -> XCSConn
    (False, _) -> XCSEna

findXRROI :: String -> [(RROutput, Maybe XRROutputInfo)]
          -> Maybe (RROutput, XRROutputInfo)
findXRROI k = lookup k 
            . mapMaybe (\(ix,x) -> (\y -> (xrr_oi_name y, (ix,y))) <$> x)

getAllXRROI :: Display -> XRRScreenResources
            -> IO [(RROutput, Maybe XRROutputInfo)]
getAllXRROI dpy xsr = mapM (\x -> (,) <$> pure x
                                      <*> xrrGetOutputInfo dpy xsr x)
                           (xrr_sr_outputs xsr)

----------------------------------------------------------------------- }}}
