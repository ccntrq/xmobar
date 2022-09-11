{-# LANGUAGE CPP #-}

------------------------------------------------------------------------------
-- |
-- Module: Xmobar.X11.Draw
-- Copyright: (c) 2018, 2020, 2022 Jose Antonio Ortega Ruiz
-- License: BSD3-style (see LICENSE)
--
-- Maintainer: jao@gnu.org
-- Stability: unstable
-- Portability: portable
-- Created: Sat Nov 24, 2018 18:49
--
--
-- Drawing the xmobar contents
--
------------------------------------------------------------------------------


module Xmobar.X11.Draw (drawInWin) where

import Control.Monad.IO.Class
import Control.Monad.Reader

import Graphics.X11.Xlib hiding (Segment)

import Xmobar.Config.Types
import Xmobar.Run.Parsers (Segment)
import Xmobar.Run.Actions (Action)
import Xmobar.X11.Types
import Xmobar.X11.XRender (drawBackground)

#ifdef CAIRO
import Xmobar.X11.CairoDraw
#else
import Xmobar.X11.XlibDraw
#endif

-- | Draws in and updates the window
#ifdef CAIRO
drawInWin :: Rectangle -> [[Segment]] -> X [([Action], Position, Position)]
drawInWin (Rectangle _ _ wid ht) segments = do
#else
drawInWin :: XConf -> Rectangle -> [[Segment]] -> X [([Action], Position, Position)]
drawInWin conf bound@(Rectangle _ _ wid ht) segments = do
#endif
  r <- ask
  let d = display r
      w = window r

      depth = defaultDepthOfScreen (defaultScreenOfDisplay d)
  p <- liftIO $ createPixmap d w wid ht depth
  gc <- liftIO $ createGC d w
  liftIO $ setGraphicsExposures d gc False

#if defined(XFT) || defined(CAIRO)
  let conf = config r
      alph = alpha conf
  when (alph < 255)
     (liftIO $ drawBackground d p (bgColor conf) alph (Rectangle 0 0 wid ht))
#endif

#ifdef CAIRO
  res <- drawInPixmap p wid ht segments
#else
  res <- liftIO $ updateActions conf bound segments
  drawInPixmap gc p wid ht segments
#endif
  -- copy the pixmap with the new string to the window
  liftIO $ copyArea d p w gc 0 0 wid ht 0 0
  -- free up everything (we do not want to leak memory!)
  liftIO $ freeGC d gc
  liftIO $ freePixmap d p
  -- resync (discard events, we don't read/process events from this display conn)
  liftIO $ sync d True
  return res
