{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Effectful.Log.Extra (
  localLogger,
  runStdErrLogger,
  withFileLogger,
  withFileLoggerEff,
  withHandleLoggerEff,
  runFileLogger,
  withStderrLogger,
  module Effectful.Log,
) where

import Control.Exception.Safe
import Control.Monad.IO.Class (MonadIO (..))
import Data.Aeson.Encode.Pretty
import Data.Aeson.Types (emptyObject)
import qualified Data.ByteString.Lazy as LBS
import Data.List.NonEmpty (NonEmpty (..))
import Data.Semigroup.Foldable (intercalateMap1)
import Data.String (IsString (..))
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.IO as T
import Data.Time (TimeZone (..), UTCTime (..), defaultTimeLocale, getCurrentTimeZone, utcToZonedTime)
import Data.Time.Calendar (Day (..))
import Data.Time.Format (formatTime)
import Effectful (Eff, IOE, (:>))
import Effectful.Dispatch.Static (localStaticRep, unsafeEff_)
import Effectful.FileSystem (FileSystem)
import qualified Effectful.FileSystem.IO as Eff
import Effectful.Log
import Effectful.Time (Clock)
import qualified Effectful.Time as Time
import GHC.IO.Device (isTerminal)
import GHC.IO.Handle.FD (handleToFd)
import Path.Tagged (File, PathTo, toFilePath)
import System.Exit (exitFailure)
import System.IO (BufferMode (..), Handle, IOMode (AppendMode), hClose, hSetBuffering, openFile, stderr)
import qualified Text.Builder as TBL
import Unsafe.Coerce (unsafeCoerce)

localLogger :: (Log :> es) => (Logger -> Logger) -> Eff es a -> Eff es a
localLogger f = localStaticRep @Log $ unsafeCoerce modify
  where
    modify :: LoggerEnv -> LoggerEnv
    modify le = le {leLogger = f (leLogger le)}

runStdErrLogger ::
  (IOE :> es) =>
  Text ->
  LogLevel ->
  Eff (Log ': es) a ->
  Eff es a
runStdErrLogger name logLevel act = withStderrLogger name $ \logger ->
  runLog name logger logLevel act

runFileLogger ::
  (IOE :> es) =>
  Text ->
  PathTo e r File ->
  LogLevel ->
  Eff (Log ': es) a ->
  Eff es a
runFileLogger name fp logLevel act = withFileLogger name fp $ \logger ->
  runLog name logger logLevel act

timestampLength :: Int
timestampLength =
  length
    ( formatTime
        defaultTimeLocale
        "%F %T.000000"
        ( utcToZonedTime
            ( TimeZone
                { timeZoneSummerOnly = False
                , timeZoneMinutes = 9 * 60
                , timeZoneName = "JST"
                }
            )
            (UTCTime (ModifiedJulianDay 0) 0)
        )
    )

withFileLogger ::
  (MonadMask m, MonadIO m) =>
  Text ->
  PathTo e r File ->
  (Logger -> m a) ->
  m a
withFileLogger name fp act =
  bracket (liftIO $ openFile (toFilePath fp) AppendMode) (liftIO . hClose) $ \h ->
    withHandleLogger name h act

withFileLoggerEff ::
  (FileSystem :> es, Clock :> es) =>
  Text ->
  PathTo e r File ->
  (Logger -> Eff es ()) ->
  Eff es ()
withFileLoggerEff name fp act =
  bracket (Eff.openFile (toFilePath fp) AppendMode) Eff.hClose $ \h ->
    withHandleLoggerEff name h act

withHandleLogger ::
  (MonadMask m, MonadIO m) => Text -> Handle -> (Logger -> m a) -> m a
withHandleLogger name h act = do
  term <- liftIO $ isTerminal =<< handleToFd h
  liftIO $ hSetBuffering h LineBuffering
  zone <- liftIO getCurrentTimeZone
  bracket
    (liftIO $ mkLogger "handle" (T.hPutStrLn h . renderLogMessage term zone Nothing))
    (\l -> liftIO $ waitForLogger l >> shutdownLogger l)
    $ \logger ->
      act logger `catchAny` \(SomeException exc) -> do
        liftIO $
          runLogT name logger LogTrace $
            logAttention_ $
              "Exception: " <> T.pack (displayException exc)
        liftIO exitFailure

withHandleLoggerEff ::
  (Clock :> es, FileSystem :> es) => Text -> Handle -> (Logger -> Eff es ()) -> Eff es ()
withHandleLoggerEff name h act = do
  term <- unsafeEff_ $ isTerminal =<< handleToFd h
  Eff.hSetBuffering h LineBuffering
  zone <- Time.getCurrentTimeZone
  bracket
    (unsafeEff_ $ mkLogger "handle" (T.hPutStrLn h . renderLogMessage term zone Nothing))
    (\l -> unsafeEff_ $ waitForLogger l >> shutdownLogger l)
    $ \logger ->
      act logger `catchAny` \(SomeException exc) -> do
        unsafeEff_ $
          runLogT name logger LogTrace $
            logAttention_ $
              "Exception: " <> T.pack (displayException exc)

withStderrLogger :: (MonadMask m, MonadIO m) => Text -> (Logger -> m a) -> m a
withStderrLogger name act = do
  withHandleLogger name stderr act

renderLogMessage ::
  -- | 'True' if terminal
  Bool ->
  TimeZone ->
  Maybe UTCTime ->
  LogMessage ->
  T.Text
renderLogMessage terminal zone mInsertionTime LogMessage {..} =
  TBL.run $
    mconcat
      [ coled sndCol $ fmtTime' lmTime
      , " "
      , foldMap (\t -> coled sndCol $ "(" <> fmtTime' t <> ") ") mInsertionTime
      , coled lvlCol ("[" <> lvlTxt <> "]")
      , " "
      , "("
      , intercalateMap1 "." TBL.text $ lmComponent :| lmDomain
      , "): "
      , TBL.text lmMessage
      ]
      <> if lmData == emptyObject
        then mempty
        else " " <> textifyData lmData
  where
    textifyData =
      TBL.text
        . TE.decodeUtf8
        . LBS.toStrict
        . encodePretty' defConfig {confIndent = Spaces 2}
    (lvlCol, lvlTxt) = case lmLevel of
      LogTrace -> ("\ESC[32m", "TRACE")
      LogInfo -> ("\ESC[34m", "INFO")
      LogAttention -> ("\ESC[31m", "ATTENTION")
    sndCol = "\ESC[90m"
    coled col xs
      | terminal = col <> xs <> "\ESC[0m"
      | otherwise = xs
    fmtTime' = fromString . take timestampLength . formatTime defaultTimeLocale "%F %T.%q" . utcToZonedTime zone
