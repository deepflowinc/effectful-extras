{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeOperators #-}

module Effectful.Process.Typed.Log (
  LogDomain,
  delegateOutputAndErrorToLog,
  runProcessLogged,
  runProcessLogged_,
) where

import Control.Monad (forever)
import Data.Function ((&))
import Data.Text (Text)
import qualified Data.Text.IO as TIO
import Effectful
import Effectful.Dispatch.Static (unsafeConcUnliftIO)
import Effectful.Log.Extra
import Effectful.Process.Typed
import qualified UnliftIO.Async as A

type LogDomain = Text

askDelegateLogStream :: (Log :> es) => Eff es (Text -> StreamSpec streamType ())
askDelegateLogStream = unsafeConcUnliftIO Persistent Unlimited $ \runInIO ->
  pure $ \dom -> mkPipeStreamSpec $ \_ h -> do
    key <- A.async $ forever $ do
      line <- TIO.hGetLine h
      runInIO $ localDomain dom $ logTrace_ line
    pure ((), A.cancel key)

delegateOutputAndErrorToLog ::
  (Log :> es) =>
  ProcessConfig stdin stdoutIgnored stderrIgnored ->
  Eff es (ProcessConfig stdin () ())
delegateOutputAndErrorToLog pc = do
  logSt <- askDelegateLogStream
  pure $ pc & setStdout (logSt "stdout") & setStderr (logSt "stderr")

runProcessLogged_ ::
  (TypedProcess :> es, Log :> es) =>
  LogDomain ->
  ProcessConfig stdin stdoutIgnored stderrIgnored ->
  Eff es ()
runProcessLogged_ dom pc =
  localDomain dom $ runProcess_ =<< delegateOutputAndErrorToLog pc

runProcessLogged ::
  (TypedProcess :> es, Log :> es) =>
  LogDomain ->
  ProcessConfig stdin stdoutIgnored stderrIgnored ->
  Eff es ExitCode
runProcessLogged dom pc =
  localDomain dom $ runProcess =<< delegateOutputAndErrorToLog pc
