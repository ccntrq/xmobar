{-# LANGUAGE CPP #-}

-----------------------------------------------------------------------------

-----------------------------------------------------------------------------

{- |
Module      :  Plugins.Monitors.ArchUpdates
Copyright   :  (c) 2024 Enrico Maria De Angelis
License     :  BSD-style (see LICENSE)

Maintainer  :  Enrico Maria De Angelis <enricomaria.dean6elis@gmail.com>
Stability   :  unstable
Portability :  unportable

An ArchLinux updates availablility plugin for Xmobar
-}
module Xmobar.Plugins.ArchUpdates (
  ArchUpdates (..),
) where

import System.Exit (ExitCode (..))
import System.Process (readProcessWithExitCode)
import Xmobar.Plugins.Command (Rate)
import Xmobar.Run.Exec

data ArchUpdates
  = ArchUpdates (String, String, String) Rate
  | PacmanUpdates
      ( String
      , String
      , String
      , String
      )
      Rate
  deriving (Read, Show)

instance Exec ArchUpdates where
  alias (ArchUpdates _ _) = "arch"
  alias (PacmanUpdates _ _) = "pacman"
  rate (ArchUpdates _ r) = r
  rate (PacmanUpdates _ r) = r
  run = archUpdatesRun

archUpdatesRun :: ArchUpdates -> IO String
archUpdatesRun au = case au of
  ArchUpdates (z, o, m) _ -> go (z, o, m, "pacman: Unknown cause of failure.")
  PacmanUpdates p _ -> go p
 where
  go (z, o, m, e) = do
    (exit, stdout, _) <- readProcessWithExitCode "checkupdates" [] ""
    return $ case exit of
      ExitFailure 2 -> z -- ero updates
      ExitFailure 1 -> e
      ExitSuccess -> case length $ lines stdout of
        0 -> impossible
        1 -> o
        n -> m >>= \c -> if c == '?' then show n else pure c
      _ -> impossible
  impossible = error "This is impossible based on pacman manpage"
