{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module Effectful.FileSystem.Glob (
  Pattern,
  globDir1,
  globDirFiles1,
  globDirDirs1,
  compile,
  globDirFilesRel1,
) where

import Control.Monad
import Data.Either (partitionEithers)
import Data.Maybe
import Effectful
import Effectful.Dispatch.Static (unsafeEff_)
import Effectful.FileSystem (FileSystem)
import Effectful.FileSystem qualified as Eff
import Path.Tagged
import Path.Tagged.IO (makeAbsolute, makeRelative)
import Path.Tagged.IO qualified as P
import System.FilePath.Glob (Pattern, compile)
import System.FilePath.Glob qualified as Glob

globDir1 ::
  (FileSystem :> es) =>
  Pattern ->
  PathTo d b Dir ->
  Eff es ([PathTo e Abs Dir], [PathTo e Abs File])
globDir1 pat dir = do
  rawFPs <- unsafeEff_ $ Glob.globDir1 pat . fromAbsDir =<< P.makeAbsolute dir
  fmap partitionEithers $ forM rawFPs $ \rawFP -> do
    isFile <- Eff.doesFileExist rawFP
    if isFile
      then pure $ Right $ fromJust $ parseAbsFile rawFP
      else pure $ Left $ fromJust $ parseAbsDir rawFP

globDirFiles1 ::
  forall e es d b.
  (FileSystem :> es) =>
  Pattern ->
  PathTo d b Dir ->
  Eff es [PathTo e Abs File]
globDirFiles1 pat dir = do
  rawFPs <- unsafeEff_ $ Glob.globDir1 pat . fromAbsDir =<< P.makeAbsolute dir
  fmap catMaybes $ forM rawFPs $ \rawFP -> do
    isFile <- Eff.doesFileExist rawFP
    if isFile
      then pure $ Just $ fromJust $ parseAbsFile rawFP
      else pure Nothing

globDirFilesRel1 ::
  forall e es d b.
  (FileSystem :> es) =>
  Pattern ->
  PathTo d b Dir ->
  Eff es [PathTo e (RelTo d) File]
globDirFilesRel1 pat dir = do
  adir <- unsafeEff_ $ makeAbsolute dir
  rawFPs <- unsafeEff_ $ Glob.globDir1 pat . fromAbsDir =<< P.makeAbsolute dir
  fmap catMaybes $ forM rawFPs $ \rawFP -> do
    isFile <- Eff.doesFileExist rawFP
    if isFile
      then pure $ Just $ fromJust $ makeRelative adir =<< parseAbsFile rawFP
      else pure Nothing

globDirDirs1 ::
  (FileSystem :> es) =>
  Pattern ->
  PathTo d b Dir ->
  Eff es [PathTo e Abs Dir]
globDirDirs1 pat dir = do
  rawFPs <- unsafeEff_ $ Glob.globDir1 pat . fromAbsDir =<< P.makeAbsolute dir
  fmap catMaybes $ forM rawFPs $ \rawFP -> do
    isDir <- Eff.doesDirectoryExist rawFP
    if isDir
      then pure $ Just $ fromJust $ parseAbsDir rawFP
      else pure Nothing
