{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module Effectful.FileSystem.Lukko (
  withFileLock,
  module Lukko,
) where

import Control.Exception.Safe
import Effectful
import Effectful.Dispatch.Static (unsafeEff_)
import Effectful.FileSystem (FileSystem)
import Effectful.FileSystem.Extra (createDirIfMissing)
import Lukko
import Path.Tagged

withFileLock ::
  (FileSystem :> es) =>
  PathTo p Abs File ->
  Eff es a ->
  Eff es a
withFileLock lockFile act = do
  createDirIfMissing True $ parent lockFile
  bracket
    ( unsafeEff_ $ do
        fd <- fdOpen (fromAbsFile lockFile)
        fd <$ fdLock fd ExclusiveLock
    )
    (\fd -> unsafeEff_ $ fdUnlock fd >> fdClose fd)
    $ const act
