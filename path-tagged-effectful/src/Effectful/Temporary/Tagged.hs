{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module Effectful.Temporary.Tagged (
  withTempFile,
  withTempDir,
  withSystemTempFile,
  withSystemTempDir,
  openTempFile,
  openBinaryTempFile,
  createTempDir,

  -- * Re-exports
  Temporary,
  runTemporary,
) where

import Effectful
import Effectful.Dispatch.Static
import Effectful.Temporary (Temporary, runTemporary)
import Path.Tagged
import Path.Tagged.IO (TempDir)
import qualified Path.Tagged.IO as P
import System.IO (Handle)

withSystemTempDir ::
  (Temporary :> es) => String -> (PathTo TempDir Abs Dir -> Eff es a) -> Eff es a
withSystemTempDir seed f = unsafeEff $ \env ->
  P.withSystemTempDir seed $
    flip unEff env . f

withTempDir ::
  (Temporary :> es) =>
  PathTo e b Dir ->
  String ->
  (PathTo TempDir Abs Dir -> Eff es a) ->
  Eff es a
withTempDir dir seed f = unsafeEff $ \env ->
  P.withTempDir dir seed $
    flip unEff env . f

withTempFile ::
  (Temporary :> es) =>
  PathTo e0 b Dir ->
  String ->
  (PathTo e Abs File -> Handle -> Eff es a) ->
  Eff es a
withTempFile dir seed f = unsafeEff $ \env ->
  P.withTempFile dir seed $
    fmap (`unEff` env) . f

withSystemTempFile ::
  (Temporary :> es) =>
  String ->
  (PathTo e Abs File -> Handle -> Eff es a) ->
  Eff es a
withSystemTempFile seed f = unsafeEff $ \env ->
  P.withSystemTempFile seed $
    fmap (`unEff` env) . f

openTempFile ::
  (Temporary :> es) =>
  PathTo e0 b Dir ->
  String ->
  Eff es (PathTo e Abs File, Handle)
{-# INLINE openTempFile #-}
openTempFile = fmap unsafeEff_ . P.openTempFile

openBinaryTempFile ::
  (Temporary :> es) =>
  PathTo e0 b Dir ->
  String ->
  Eff es (PathTo e Abs File, Handle)
{-# INLINE openBinaryTempFile #-}
openBinaryTempFile = fmap unsafeEff_ . P.openBinaryTempFile

createTempDir ::
  (Temporary :> es) =>
  PathTo e b Dir ->
  String ->
  Eff es (PathTo TempDir Abs Dir)
{-# INLINE createTempDir #-}
createTempDir = fmap unsafeEff_ . P.createTempDir
