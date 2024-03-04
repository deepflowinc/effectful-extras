{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeOperators #-}

module Effectful.Temporary.Extra (withSystemTempDir, withTempDir) where

import Data.Maybe (fromJust)
import Effectful
import Effectful.Temporary
import Path.Tagged

withSystemTempDir ::
  (Temporary :> es) => String -> (PathTo tag Abs Dir -> Eff es a) -> Eff es a
withSystemTempDir seed f =
  withSystemTempDirectory seed $
    f . fromJust . parseAbsDir

withTempDir ::
  (Temporary :> es) => PathTo (tag0 :: k) Abs Dir -> String -> (PathTo tag Abs Dir -> Eff es a) -> Eff es a
withTempDir dir seed f =
  withTempDirectory (fromAbsDir dir) seed $
    f . fromJust . parseAbsDir
