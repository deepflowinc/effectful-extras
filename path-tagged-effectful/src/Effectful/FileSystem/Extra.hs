{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module Effectful.FileSystem.Extra (
  hPutBuilder,
  hPutBinaryLazy,
  hPutBinaryStrict,
  writeBuilder,
  writeBinaryStrict,
  setFileMode,
  removeFile,
  removeDirRecur,
  FileMode,
  createDirIfMissing,
  copyFile,
  doesFileExist,
  doesDirExist,
  hPutText,
  copyDirRecur,
  makeAbsolute,
  listDirRel,
  listDir,
  listDirRecur,
  listDirRecurRel,
  readFileBinaryLazy,
  readFileBinaryStrict,
  walkDirRel,

  -- * Re-exports
  WalkAction (..),
  -- makeAbsolute,
) where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Effectful
import Effectful.Dispatch.Static (unEff, unsafeEff, unsafeEff_)
import Effectful.FileSystem hiding (copyFile, doesFileExist, makeAbsolute, removeFile)
import Path.Tagged
import Path.Tagged.IO (WalkAction)
import qualified Path.Tagged.IO as P
import System.IO (Handle)
import qualified System.Posix.Files as Posix
import System.Posix.Types (FileMode)

hPutBuilder :: (FileSystem :> es) => Handle -> BB.Builder -> Eff es ()
hPutBuilder = fmap unsafeEff_ . BB.hPutBuilder

hPutBinaryLazy :: (FileSystem :> es) => Handle -> LBS.ByteString -> Eff es ()
hPutBinaryLazy = fmap unsafeEff_ . LBS.hPut

hPutBinaryStrict :: (FileSystem :> es) => Handle -> BS.ByteString -> Eff es ()
hPutBinaryStrict = fmap unsafeEff_ . BS.hPut

readFileBinaryLazy :: (FileSystem :> es) => PathTo e b File -> Eff es LBS.ByteString
readFileBinaryLazy = unsafeEff_ . LBS.readFile . toFilePath

readFileBinaryStrict :: (FileSystem :> es) => PathTo e b File -> Eff es BS.ByteString
readFileBinaryStrict = unsafeEff_ . BS.readFile . toFilePath

hPutText :: (FileSystem :> es) => Handle -> T.Text -> Eff es ()
{-# INLINE hPutText #-}
hPutText = fmap unsafeEff_ . TIO.hPutStr

writeBuilder :: (FileSystem :> es) => PathTo e b File -> BB.Builder -> Eff es ()
{-# INLINE writeBuilder #-}
writeBuilder = fmap unsafeEff_ . BB.writeFile . toFilePath

writeBinaryStrict :: (FileSystem :> es) => PathTo e b File -> BS.ByteString -> Eff es ()
{-# INLINE writeBinaryStrict #-}
writeBinaryStrict = fmap unsafeEff_ . BS.writeFile . toFilePath

setFileMode :: (FileSystem :> es) => PathTo e b k -> FileMode -> Eff es ()
setFileMode = fmap unsafeEff_ . Posix.setFileMode . toFilePath

createDirIfMissing :: (FileSystem :> es) => Bool -> PathTo e b Dir -> Eff es ()
createDirIfMissing = fmap unsafeEff_ . P.createDirIfMissing

doesDirExist :: (FileSystem :> es) => PathTo e b Dir -> Eff es Bool
doesDirExist = unsafeEff_ . P.doesDirExist

doesFileExist :: (FileSystem :> es) => PathTo e b File -> Eff es Bool
doesFileExist = unsafeEff_ . P.doesFileExist

copyFile :: (FileSystem :> es) => PathTo e0 b0 File -> PathTo e1 b1 File -> Eff es ()
{-# INLINE copyFile #-}
copyFile = fmap unsafeEff_ . P.copyFile

copyDirRecur :: (FileSystem :> es) => PathTo e0 b0 Dir -> PathTo e1 b1 Dir -> Eff es ()
copyDirRecur = fmap unsafeEff_ . P.copyDirRecur

makeAbsolute ::
  (FileSystem :> es, P.AnyPathTo path) =>
  path ->
  Eff es (P.AbsPath path)
makeAbsolute = unsafeEff_ . P.makeAbsolute

listDirRel ::
  (FileSystem :> es) =>
  PathTo e b Dir ->
  Eff
    es
    ([PathTo Unknown (RelTo e) Dir], [PathTo Unknown (RelTo e) File])
listDirRel = unsafeEff_ . P.listDirRel

listDir ::
  (FileSystem :> es) =>
  PathTo e b Dir ->
  Eff
    es
    ([PathTo Unknown Abs Dir], [PathTo Unknown Abs File])
listDir = unsafeEff_ . P.listDir

listDirRecurRel ::
  (FileSystem :> es) =>
  PathTo e b Dir ->
  Eff
    es
    ([PathTo Unknown (RelTo e) Dir], [PathTo Unknown (RelTo e) File])
listDirRecurRel = unsafeEff_ . P.listDirRecurRel

listDirRecur ::
  (FileSystem :> es) =>
  PathTo e b Dir ->
  Eff
    es
    ([PathTo Unknown Abs Dir], [PathTo Unknown Abs File])
listDirRecur = unsafeEff_ . P.listDirRecur

walkDirRel ::
  (FileSystem :> es) =>
  ( forall dir.
    PathTo dir (RelTo e) Dir ->
    [PathTo Unknown (RelTo dir) Dir] ->
    [PathTo Unknown (RelTo dir) File] ->
    Eff es (WalkAction (RelTo dir))
  ) ->
  PathTo e b Dir ->
  Eff es ()
walkDirRel walk super = unsafeEff $ \env -> do
  P.walkDirRel
    ( \p dirs files ->
        unEff (walk p dirs files) env
    )
    super

removeFile :: (FileSystem :> es) => PathTo e b File -> Eff es ()
removeFile = unsafeEff_ . P.removeFile

removeDirRecur :: (FileSystem :> es) => PathTo e b Dir -> Eff es ()
removeDirRecur = unsafeEff_ . P.removeDirRecur
