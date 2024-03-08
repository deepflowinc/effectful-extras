{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module Effectful.FileSystem.Tagged (
  -- * Actions on directories
  createDir,
  createDirIfMissing,
  ensureDir,
  removeDir,
  removeDirRecur,
  removePathForcibly,
  renameDir,
  renamePath,
  listDir,
  listDirRel,
  listDirRecur,
  listDirRecurRel,
  copyDirRecur,
  copyDirRecur',

  -- * Walking directory trees
  WalkAction (..),
  walkDir,
  walkDirRel,
  walkDirAccum,
  walkDirAccumRel,

  -- * Current working directory
  getCurrentDir,
  setCurrentDir,
  withCurrentDir,

  -- * Pre-defined directories
  PredefinedDir (..),
  WithPredefined,
  Cwd,
  Home,
  AppUserData,
  UserDocs,
  TempDir,
  getHomeDir,
  getAppUserDataDir,
  getUserDocsDir,
  getTempDir,
  XdgDirectory (..),
  XdgData,
  XdgConfig,
  XdgCache,
  WithXdg,
  KnownXdgDirectory,
  getXdgBaseDir,
  getXdgDir,
  getXdgDataDirs,
  getXdgConfigDirs,

  -- * Path transformation
  AnyPathTo (PathTag, AbsPath, RelPathTo'),
  RelPathTo,
  canonicalizePath,
  makeAbsolute,
  makeRelative,
  makeRelativeToCurrentDir,
  resolveFile,
  resolveFile',
  resolveDir,
  resolveDir',
  findExecutable,
  findFile,
  findFiles,
  findFilesWith,

  -- * Actions on Files
  removeFile,
  renameFile,
  copyFile,
  getFileSize,
  FileMode,

  -- * Symbolic Links
  createFileLink,
  createDirLink,
  removeDirLink,
  getSymlinkTarget,
  isSymlink,

  -- * Existence tests
  doesPathExist,
  doesFileExist,
  doesDirExist,
  isLocationOccupied,
  forgivingAbsence,
  ignoreAbsence,

  -- * Permissions
  Permissions,
  emptyPermissions,
  getPermissions,
  setPermissions,
  copyPermissions,

  -- * Posix File Modes
  getFileMode,
  setFileMode,

  -- * Timestamps
  getAccessTime,
  setAccessTime,
  getModificationTime,
  setModificationTime,

  -- * File I/O
  hPutBuilder,
  hPutBinaryStrict,
  hPutBinaryLazy,
  hPutTextStrict,
  hPutTextLazy,
  writeBuilder,
  writeBinaryStrict,
  writeBinaryLazy,
  writeTextStrict,
  writeTextLazy,
  readFileBinaryLazy,
  readFileBinaryStrict,
  readFileTextLazy,
  readFileTextStrict,

  -- * Re-exports
  FileSystem,
  runFileSystem,
) where

import Control.Exception.Safe (bracket, catch, throwM)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Lazy as LBS
import Data.Functor (void, (<&>))
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.IO as LTIO
import Data.Time (UTCTime)
import Effectful
import Effectful.Dispatch.Static (unEff, unsafeEff, unsafeEff_)
import Effectful.FileSystem (FileSystem, runFileSystem)
import GHC.Stack (HasCallStack, withFrozenCallStack)
import Path.Tagged
import Path.Tagged.IO (
  AnyPathTo (AbsPath, PathTag, RelPathTo'),
  AppUserData,
  Cwd,
  Home,
  KnownXdgDirectory,
  Permissions,
  PredefinedDir (..),
  RelPathTo,
  TempDir,
  UserDocs,
  WalkAction,
  WithPredefined,
  WithXdg,
  XdgCache,
  XdgConfig,
  XdgData,
  XdgDirectory (..),
  emptyPermissions,
 )
import qualified Path.Tagged.IO as P
import System.IO (Handle)
import System.IO.Error (isDoesNotExistError)
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

readFileTextLazy :: (FileSystem :> es) => PathTo e b File -> Eff es LT.Text
readFileTextLazy = unsafeEff_ . LTIO.readFile . toFilePath

readFileTextStrict :: (FileSystem :> es) => PathTo e b File -> Eff es T.Text
readFileTextStrict = unsafeEff_ . TIO.readFile . toFilePath

hPutTextStrict :: (FileSystem :> es) => Handle -> T.Text -> Eff es ()
{-# INLINE hPutTextStrict #-}
hPutTextStrict = fmap unsafeEff_ . TIO.hPutStr

hPutTextLazy :: (FileSystem :> es) => Handle -> LT.Text -> Eff es ()
{-# INLINE hPutTextLazy #-}
hPutTextLazy = fmap unsafeEff_ . LTIO.hPutStr

writeBuilder :: (FileSystem :> es) => PathTo e b File -> BB.Builder -> Eff es ()
{-# INLINE writeBuilder #-}
writeBuilder = fmap unsafeEff_ . BB.writeFile . toFilePath

writeBinaryStrict :: (FileSystem :> es) => PathTo e b File -> BS.ByteString -> Eff es ()
{-# INLINE writeBinaryStrict #-}
writeBinaryStrict = fmap unsafeEff_ . BS.writeFile . toFilePath

writeBinaryLazy :: (FileSystem :> es) => PathTo e b File -> LBS.ByteString -> Eff es ()
{-# INLINE writeBinaryLazy #-}
writeBinaryLazy = fmap unsafeEff_ . LBS.writeFile . toFilePath

writeTextStrict :: (FileSystem :> es) => PathTo e b File -> T.Text -> Eff es ()
{-# INLINE writeTextStrict #-}
writeTextStrict = fmap unsafeEff_ . TIO.writeFile . toFilePath

writeTextLazy :: (FileSystem :> es) => PathTo e b File -> LT.Text -> Eff es ()
{-# INLINE writeTextLazy #-}
writeTextLazy = fmap unsafeEff_ . LTIO.writeFile . toFilePath

setFileMode :: (FileSystem :> es) => PathTo e b k -> FileMode -> Eff es ()
{-# INLINE setFileMode #-}
setFileMode = fmap unsafeEff_ . Posix.setFileMode . toFilePath

getFileMode :: (FileSystem :> es) => PathTo e b k -> Eff es FileMode
{-# INLINE getFileMode #-}
getFileMode = unsafeEff_ . fmap Posix.fileMode . Posix.getFileStatus . toFilePath

createDir :: (FileSystem :> es) => PathTo e b Dir -> Eff es ()
{-# INLINE createDir #-}
createDir = unsafeEff_ . P.createDir

createDirIfMissing :: (FileSystem :> es) => Bool -> PathTo e b Dir -> Eff es ()
{-# INLINE createDirIfMissing #-}
createDirIfMissing = fmap unsafeEff_ . P.createDirIfMissing

ensureDir :: (FileSystem :> es) => PathTo e b Dir -> Eff es ()
{-# INLINE ensureDir #-}
ensureDir = createDirIfMissing True

doesDirExist :: (FileSystem :> es) => PathTo e b Dir -> Eff es Bool
{-# INLINE doesDirExist #-}
doesDirExist = unsafeEff_ . P.doesDirExist

doesPathExist :: (FileSystem :> es) => PathTo e b t -> Eff es Bool
{-# INLINE doesPathExist #-}
doesPathExist = unsafeEff_ . P.doesPathExist

isLocationOccupied :: (FileSystem :> es) => PathTo e b t -> Eff es Bool
{-# INLINE isLocationOccupied #-}
isLocationOccupied = unsafeEff_ . P.isLocationOccupied

{- | If argument of the function throws a
'System.IO.Error.doesNotExistErrorType', 'Nothing' is returned (other
exceptions propagate). Otherwise the result is returned inside a 'Just'.
-}
forgivingAbsence ::
  (HasCallStack, FileSystem :> es) =>
  Eff es a ->
  Eff es (Maybe a)
forgivingAbsence f =
  withFrozenCallStack
    catch
    (Just <$> f)
    (\e -> if isDoesNotExistError e then pure Nothing else throwM e)

-- | The same as 'forgivingAbsence', but ignores result.
ignoreAbsence ::
  (HasCallStack, FileSystem :> es) =>
  Eff es a ->
  Eff es ()
{-# INLINE ignoreAbsence #-}
ignoreAbsence = void . forgivingAbsence

doesFileExist :: (FileSystem :> es) => PathTo e b File -> Eff es Bool
{-# INLINE doesFileExist #-}
doesFileExist = unsafeEff_ . P.doesFileExist

copyFile :: (FileSystem :> es) => PathTo e0 b0 File -> PathTo e1 b1 File -> Eff es ()
{-# INLINE copyFile #-}
copyFile = fmap unsafeEff_ . P.copyFile

getFileSize :: (FileSystem :> es) => PathTo e b File -> Eff es Integer
{-# INLINE getFileSize #-}
getFileSize = unsafeEff_ . P.getFileSize

findExecutable :: (FileSystem :> es) => PathTo e (RelTo b) File -> Eff es (Maybe (PathTo e Abs File))
{-# INLINE findExecutable #-}
findExecutable = unsafeEff_ . P.findExecutable

findFile ::
  (FileSystem :> es) =>
  [PathTo dir b Dir] ->
  PathTo e (RelTo dir) File ->
  Eff es (Maybe (PathTo e Abs File))
{-# INLINE findFile #-}
findFile = fmap unsafeEff_ . P.findFile

findFiles ::
  (FileSystem :> es) =>
  [PathTo dir b Dir] ->
  PathTo e (RelTo dir) File ->
  Eff es [PathTo e Abs File]
{-# INLINE findFiles #-}
findFiles = fmap unsafeEff_ . P.findFiles

findFilesWith ::
  (FileSystem :> es) =>
  (PathTo e Abs File -> Eff es Bool) ->
  [PathTo dir b Dir] ->
  PathTo e (RelTo dir) File ->
  Eff es [PathTo e Abs File]
{-# INLINE findFilesWith #-}
findFilesWith p dirs targ = unsafeEff $ \env ->
  P.findFilesWith
    (\file -> unEff (p file) env)
    dirs
    targ

copyDirRecur :: (FileSystem :> es) => PathTo e0 b0 Dir -> PathTo e1 b1 Dir -> Eff es ()
copyDirRecur = fmap unsafeEff_ . P.copyDirRecur

-- | The same as copyDirRecur, but it does not preserve directory permissions.
copyDirRecur' :: (FileSystem :> es) => PathTo e0 b0 Dir -> PathTo e1 b1 Dir -> Eff es ()
copyDirRecur' = fmap unsafeEff_ . P.copyDirRecur'

makeAbsolute ::
  (FileSystem :> es, AnyPathTo path) =>
  path ->
  Eff es (AbsPath path)
makeAbsolute = unsafeEff_ . P.makeAbsolute

canonicalizePath ::
  (FileSystem :> es, AnyPathTo path) =>
  path ->
  Eff es (AbsPath path)
canonicalizePath = unsafeEff_ . P.canonicalizePath

makeRelative ::
  (FileSystem :> es, AnyPathTo path) =>
  PathTo (e :: PathTag path) Abs Dir ->
  path ->
  Eff es (RelPathTo e path)
makeRelative = fmap unsafeEff_ . P.makeRelative

makeRelativeToCurrentDir ::
  (FileSystem :> es, AnyPathTo path) =>
  path ->
  Eff es (RelPathTo' (PathTag path) Cwd path)
makeRelativeToCurrentDir = unsafeEff_ . P.makeRelativeToCurrentDir

resolveFile ::
  (FileSystem :> es) =>
  PathTo e0 Abs Dir ->
  FilePath ->
  Eff es (PathTo e Abs File)
resolveFile = fmap unsafeEff_ . P.resolveFile

resolveFile' ::
  (FileSystem :> es) =>
  FilePath ->
  Eff es (PathTo e Abs File)
resolveFile' = unsafeEff_ . P.resolveFile'

resolveDir ::
  (FileSystem :> es) =>
  PathTo e0 Abs Dir ->
  FilePath ->
  Eff es (PathTo e Abs Dir)
resolveDir = fmap unsafeEff_ . P.resolveDir

resolveDir' ::
  (FileSystem :> es) =>
  FilePath ->
  Eff es (PathTo e Abs Dir)
resolveDir' = unsafeEff_ . P.resolveDir'

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

walkDir ::
  (FileSystem :> es) =>
  ( forall dir.
    PathTo dir Abs Dir ->
    [PathTo Unknown Abs Dir] ->
    [PathTo Unknown Abs File] ->
    Eff es (WalkAction Abs)
  ) ->
  PathTo e b Dir ->
  Eff es ()
walkDir walk super = unsafeEff $ \env -> do
  P.walkDir
    ( \p dirs files ->
        unEff (walk p dirs files) env
    )
    super

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

walkDirAccum ::
  (FileSystem :> es, Monoid o) =>
  Maybe
    ( forall dir.
      PathTo dir Abs Dir ->
      [PathTo Unknown Abs Dir] ->
      [PathTo Unknown Abs File] ->
      Eff es (WalkAction Abs)
    ) ->
  ( forall dir.
    PathTo dir Abs Dir ->
    [PathTo Unknown Abs Dir] ->
    [PathTo Unknown Abs File] ->
    Eff es o
  ) ->
  PathTo e b Dir ->
  Eff es o
walkDirAccum act f super =
  unsafeEff $ \env -> do
    P.walkDirAccum
      ( act <&> \g p dirs files ->
          unEff (g p dirs files) env
      )
      (\p dirs files -> unEff (f p dirs files) env)
      super

walkDirAccumRel ::
  (FileSystem :> es, Monoid o) =>
  Maybe
    ( forall dir.
      PathTo dir (RelTo e) Dir ->
      [PathTo Unknown (RelTo dir) Dir] ->
      [PathTo Unknown (RelTo dir) File] ->
      Eff es (WalkAction (RelTo dir))
    ) ->
  ( forall dir.
    PathTo dir (RelTo e) Dir ->
    [PathTo Unknown (RelTo dir) Dir] ->
    [PathTo Unknown (RelTo dir) File] ->
    Eff es o
  ) ->
  PathTo e b Dir ->
  Eff es o
walkDirAccumRel act f super =
  unsafeEff $ \env -> do
    P.walkDirAccumRel
      ( act <&> \g p dirs files ->
          unEff (g p dirs files) env
      )
      (\p dirs files -> unEff (f p dirs files) env)
      super

removeFile :: (FileSystem :> es) => PathTo e b File -> Eff es ()
{-# INLINE removeFile #-}
removeFile = unsafeEff_ . P.removeFile

renameFile :: (FileSystem :> es) => PathTo e b0 File -> PathTo e b1 File -> Eff es ()
{-# INLINE renameFile #-}
renameFile = fmap unsafeEff_ . P.renameFile

removeDir :: (FileSystem :> es) => PathTo e b Dir -> Eff es ()
{-# INLINE removeDir #-}
removeDir = unsafeEff_ . P.removeDir

removeDirRecur :: (FileSystem :> es) => PathTo e b Dir -> Eff es ()
{-# INLINE removeDirRecur #-}
removeDirRecur = unsafeEff_ . P.removeDirRecur

removePathForcibly :: (FileSystem :> es) => PathTo e b Dir -> Eff es ()
{-# INLINE removePathForcibly #-}
removePathForcibly = unsafeEff_ . P.removePathForcibly

renameDir :: (FileSystem :> es) => PathTo e b Dir -> PathTo e b' Dir -> Eff es ()
{-# INLINE renameDir #-}
renameDir = fmap unsafeEff_ . P.renameDir

renamePath :: (FileSystem :> es) => PathTo e b t -> PathTo e b' t -> Eff es ()
{-# INLINE renamePath #-}
renamePath = fmap unsafeEff_ . P.renamePath

withCurrentDir ::
  (FileSystem :> es) =>
  PathTo e r Dir ->
  Eff es a ->
  Eff es a
withCurrentDir dir act =
  bracket (unsafeEff_ P.getCurrentDir) (unsafeEff_ . P.setCurrentDir) $
    const $
      unsafeEff_ (P.setCurrentDir dir) >> act

getCurrentDir ::
  (FileSystem :> es) =>
  Eff es (PathTo Cwd Abs Dir)
{-# INLINE getCurrentDir #-}
getCurrentDir = unsafeEff_ P.getCurrentDir

setCurrentDir ::
  (FileSystem :> es) =>
  PathTo e b Dir ->
  Eff es ()
{-# INLINE setCurrentDir #-}
setCurrentDir = unsafeEff_ . P.setCurrentDir

getHomeDir :: (FileSystem :> es) => Eff es (PathTo Home Abs Dir)
{-# INLINE getHomeDir #-}
getHomeDir = unsafeEff_ P.getHomeDir

getAppUserDataDir :: (FileSystem :> es) => String -> Eff es (PathTo AppUserData Abs Dir)
{-# INLINE getAppUserDataDir #-}
getAppUserDataDir = unsafeEff_ . P.getAppUserDataDir

getUserDocsDir :: (FileSystem :> es) => Eff es (PathTo UserDocs Abs Dir)
{-# INLINE getUserDocsDir #-}
getUserDocsDir = unsafeEff_ P.getUserDocsDir

getTempDir :: (FileSystem :> es) => Eff es (PathTo TempDir Abs Dir)
{-# INLINE getTempDir #-}
getTempDir = unsafeEff_ P.getTempDir

getXdgBaseDir ::
  forall xdg es.
  (KnownXdgDirectory xdg, FileSystem :> es) =>
  Eff es (PathTo xdg Abs Dir)
{-# INLINE getXdgBaseDir #-}
getXdgBaseDir = unsafeEff_ P.getXdgBaseDir

getXdgDir ::
  forall xdg e es.
  ( KnownXdgDirectory xdg
  , FileSystem :> es
  ) =>
  PathTo e (RelTo (WithXdg xdg)) Dir ->
  Eff es (PathTo e Abs Dir)
{-# INLINE getXdgDir #-}
getXdgDir = unsafeEff_ . P.getXdgDir

getXdgDataDirs :: (FileSystem :> es) => Eff es [PathTo XdgData Abs Dir]
{-# INLINE getXdgDataDirs #-}
getXdgDataDirs = unsafeEff_ P.getXdgDataDirs

getXdgConfigDirs :: (FileSystem :> es) => Eff es [PathTo XdgConfig Abs Dir]
{-# INLINE getXdgConfigDirs #-}
getXdgConfigDirs = unsafeEff_ P.getXdgConfigDirs

createFileLink :: (FileSystem :> es) => PathTo e b0 File -> PathTo e b1 File -> Eff es ()
{-# INLINE createFileLink #-}
createFileLink = fmap unsafeEff_ . P.createFileLink

createDirLink :: (FileSystem :> es) => PathTo e b0 Dir -> PathTo e b1 Dir -> Eff es ()
{-# INLINE createDirLink #-}
createDirLink = fmap unsafeEff_ . P.createDirLink

removeDirLink :: (FileSystem :> es) => PathTo e b0 Dir -> Eff es ()
{-# INLINE removeDirLink #-}
removeDirLink = unsafeEff_ . P.removeDirLink

getSymlinkTarget :: (FileSystem :> es) => PathTo e b t -> Eff es FilePath
{-# INLINE getSymlinkTarget #-}
getSymlinkTarget = unsafeEff_ . P.getSymlinkTarget

isSymlink :: (FileSystem :> es) => PathTo e b t -> Eff es Bool
{-# INLINE isSymlink #-}
isSymlink = unsafeEff_ . P.isSymlink

getPermissions :: (FileSystem :> es) => PathTo e b t -> Eff es Permissions
{-# INLINE getPermissions #-}
getPermissions = unsafeEff_ . P.getPermissions

setPermissions :: (FileSystem :> es) => PathTo e b t -> Permissions -> Eff es ()
{-# INLINE setPermissions #-}
setPermissions = fmap unsafeEff_ . P.setPermissions

copyPermissions :: (FileSystem :> es) => PathTo e0 b0 t0 -> PathTo e1 b1 t1 -> Eff es ()
{-# INLINE copyPermissions #-}
copyPermissions = fmap unsafeEff_ . P.copyPermissions

getAccessTime :: (FileSystem :> es) => PathTo e b t -> Eff es UTCTime
{-# INLINE getAccessTime #-}
getAccessTime = unsafeEff_ . P.getAccessTime

setAccessTime :: (FileSystem :> es) => PathTo e b t -> UTCTime -> Eff es ()
{-# INLINE setAccessTime #-}
setAccessTime = fmap unsafeEff_ . P.setAccessTime

getModificationTime :: (FileSystem :> es) => PathTo e b t -> Eff es UTCTime
{-# INLINE getModificationTime #-}
getModificationTime = unsafeEff_ . P.getModificationTime

setModificationTime :: (FileSystem :> es) => PathTo e b t -> UTCTime -> Eff es ()
{-# INLINE setModificationTime #-}
setModificationTime = fmap unsafeEff_ . P.setModificationTime
