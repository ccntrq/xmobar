-----------------------------------------------------------------------------
-- |
-- Module      :  XMobar.Environment
-- Copyright   :  (c) William Song
-- License     :  BSD-style (see LICENSE)
--
-- Maintainer  :  Will Song <incertia@incertia.net>
-- Stability   :  stable
-- Portability :  portable
--
-- A function to expand environment variables in strings
--
-----------------------------------------------------------------------------
module Xmobar.System.Environment(expandEnv) where

import qualified Data.Maybe as M
import qualified System.Environment as E

expandEnv :: String -> IO String
expandEnv "" = return ""
expandEnv (c:s) = case c of
  '$'       -> do
    envVar <- M.fromMaybe "" <$> E.lookupEnv e
    remainder <- expandEnv s'
    return $ envVar ++ remainder
    where (e, s') = getVar s
          getVar "" = ("", "")
          getVar ('{':s'') = (takeUntil "}" s'', drop 1 . dropUntil "}" $ s'')
          getVar s'' = (takeUntil filterstr s'', dropUntil filterstr s'')
          filterstr = ",./? \t;:\"'~`!@#$%^&*()<>-+=\\|"
          takeUntil f = takeWhile (not . flip elem f)
          dropUntil f = dropWhile (not . flip elem f)

  '\\' -> case s == "" of
    True  -> return "\\"
    False -> do
      remainder <- expandEnv $ drop 1 s
      return $ escString s ++ remainder
      where escString (cc:_) =
              case cc of
                't' -> "\t"
                'n' -> "\n"
                '$' -> "$"
                _   -> [cc]
            escString [] = ""

  _    -> do
    remainder <- expandEnv s
    return $ c : remainder
