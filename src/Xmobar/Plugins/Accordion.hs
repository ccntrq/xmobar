{-# LANGUAGE TupleSections, FlexibleContexts #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Plugins.Monitors.Accordion
-- Copyright   :  (c) 2024 Enrico Maria De Angelis
-- License     :  BSD-style (see LICENSE)
--
-- Maintainer  :  Enrico Maria De Angelis <enricomaria.dean6elis@gmail.com>
-- Stability   :  unstable
-- Portability :  unportable
--
-- A plugin to group adjacent plugins and make them, as a whole, shrinkable to
-- an alternate text upon clicking.
--
-----------------------------------------------------------------------------

module Xmobar.Plugins.Accordion (defaultTuning, makeAccordion, Tuning(..)) where

import Control.Concurrent.Async (withAsync)
import Control.Exception (finally)
import Control.Monad (forever, join, when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (runReaderT, ask)
import Control.Monad.State.Strict (evalStateT, get, modify')
import Data.IORef (atomicModifyIORef', newIORef, readIORef)
import Data.Maybe (isJust)
import System.Directory (removeFile)
import System.Exit (ExitCode(..))
import System.Process (readProcessWithExitCode)
import Xmobar.Run.Exec (Exec(..), tenthSeconds)

-- TODO: Ideally, I'd have just `Accordion`, and not `Tuning`, but since
-- `Accordion` is polymorphic, I can't have a `defaultAccordion` constructor
-- with `plugins = []`, because that leaves  `a` undetermined.
-- So I have move all non-polymorphic typed members in `Tuning`, allowing for
-- default values at least for those members.
data Accordion a = Accordion {
    tuning :: Tuning
  , plugins :: [a]
} deriving (Show, Read)

makeAccordion :: Exec a => Tuning -> [a] -> Accordion a
makeAccordion t rs = Accordion { tuning = t, plugins = rs }

data Tuning = Tuning {
    alias' :: String
  , initial :: Bool
  , expand :: String
  , shrink :: String
} deriving (Read, Show)

defaultTuning :: Tuning
defaultTuning = Tuning {
    alias' = "accordion"
  , initial = True
  , expand = "<>"
  , shrink = "><"
}

instance (Exec a, Read a, Show a) => Exec (Accordion a) where
  alias (Accordion Tuning { alias' = name } _) = name
  start (Accordion Tuning { initial = initial'
                          , expand = expand'
                          , shrink = shrink' }
                   runnables)
        cb = do
    clicked <- newIORef Nothing
    (_, n, _) <- readProcessWithExitCode "uuidgen" [] ""
    let pipe = "/tmp/accordion-" ++ removeLinebreak n
    (_, _, _) <- readProcessWithExitCode "mkfifo" [pipe] ""
    withAsync (forever $ do (ret, _, _) <- readProcessWithExitCode "cat" [pipe] ""
                            case ret of
                              ExitSuccess -> atomicModifyIORef' clicked (const (Just (), ()))
                              ExitFailure _ -> error "how is this possible?")
              (const $ do
                  srefs <- mapM (newIORef . const "") runnables
                  foldr (\(runnable, sref) acc -> withAsync (start runnable (writeToRef sref)) (const acc))
                        (forever (do liftIO (tenthSeconds 1)
                                     clicked' <- liftIO $ readIORef clicked
                                     when (isJust clicked')
                                          (do liftIO $ clear clicked
                                              modify' not)
                                     b <- get
                                     if b then loop pipe else liftIO $ cb (click pipe expand'))
                                 `runReaderT` srefs `evalStateT` initial')
                        (zip runnables srefs))
      `finally` removeFile pipe
    where
      click file icon = "<action=`echo 1 > " ++ file ++ "`>" ++ icon ++ "</action>"
      clear = (`atomicModifyIORef'` const (Nothing, ()))
      removeLinebreak = init
      writeToRef strRef = atomicModifyIORef' strRef . const . (,())
      loop p = do
        srefs <- ask
        text <- join <$> mapM (liftIO . readIORef) srefs
        liftIO $ cb $ text ++ click p shrink'
