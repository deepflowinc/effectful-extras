{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module Effectful.FileSystem.Streaming (hGetContents, hPut, readFile, writeFile) where

import Control.Monad.Trans.Class (lift)
import Data.ByteString qualified as B
import Data.ByteString qualified as BS
import Data.ByteString.Builder.Extra qualified as BB
import Effectful
import Effectful.Dispatch.Static (unsafeEff_)
import Effectful.FileSystem (FileSystem)
import Effectful.FileSystem.IO (Handle, IOMode (..), hClose, withFile)
import Effectful.Resource (Resource, allocateEff, release)
import Path.Tagged
import Streaming.ByteString qualified as Q
import Streaming.ByteString.Internal qualified as Q
import System.IO (openBinaryFile)
import Prelude hiding (readFile, writeFile)

bracketBS ::
  (Resource :> es) =>
  Eff es a ->
  (a -> Eff es ()) ->
  (a -> Q.ByteStream (Eff es) b) ->
  Q.ByteStream (Eff es) b
bracketBS alloc free inside = do
  (key, seed) <- lift (allocateEff alloc free)
  clean key $ inside seed
  where
    clean key = loop
      where
        loop str = case str of
          Q.Empty r -> Q.Go (unsafeEff_ (release key) >> return (Q.Empty r))
          Q.Go m -> Q.Go (fmap loop m)
          Q.Chunk bs rest -> Q.Chunk bs (loop rest)

readFile ::
  (FileSystem :> es, Resource :> es) =>
  PathTo e b File ->
  Q.ByteStream (Eff es) ()
readFile fp =
  bracketBS (unsafeEff_ $ openBinaryFile (toFilePath fp) ReadMode) hClose hGetContents

writeFile ::
  (FileSystem :> es) =>
  PathTo e b File ->
  Q.ByteStream (Eff es) r ->
  Eff es r
writeFile fp str =
  withFile (toFilePath fp) WriteMode $ \h ->
    hPut h str

hPut :: (FileSystem :> es) => Handle -> Q.ByteStream (Eff es) r -> Eff es r
hPut h cs =
  Q.dematerialize
    cs
    return
    (\x y -> unsafeEff_ (BS.hPut h x) >> y)
    (>>= id)

hGetContents :: (FileSystem :> es) => Handle -> Q.ByteStream (Eff es) ()
hGetContents = hGetContentsN BB.defaultChunkSize

hGetContentsN :: (FileSystem :> es) => Int -> Handle -> Q.ByteStream (Eff es) ()
hGetContentsN k h = loop
  where
    loop = do
      c <- lift $ unsafeEff_ (B.hGetSome h k)
      -- only blocks if there is no data available
      if B.null c
        then Q.Empty ()
        else Q.Chunk c loop
{-# INLINEABLE hGetContentsN #-} -- very effective inline pragma
