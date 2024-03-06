{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module Effectful.Concurrent.TimedResource (
  Expiration,
  runExpiration,
  TimedResource,
  readResource,
  newTimedResource,
) where

import Control.Exception.Safe (onException)
import qualified Data.Time as UIO
import Effectful (Dispatch (..), DispatchOf, Eff, Effect, IOE, (:>))
import Effectful.Concurrent.STM
import Effectful.Dispatch.Static
import Effectful.Time (Clock, UTCTime)
import qualified UnliftIO.STM as STM

data R a = Value a | UpdateFailed

data Expiration :: Effect

type instance DispatchOf Expiration = 'Static 'WithSideEffects

data instance StaticRep Expiration = Exp

data TimedResource a = TimedResource
  { var :: TMVar (R a)
  , acquire :: IO (a, UTCTime)
  , expiresAt :: TVar UTCTime
  }

runExpiration ::
  (IOE :> es, Concurrent :> es, Clock :> es) =>
  Eff (Expiration ': es) a ->
  Eff es a
runExpiration = evalStaticRep Exp

newTimedResource ::
  (Expiration :> es) =>
  IO (a, UTCTime) ->
  Eff es (TimedResource a)
newTimedResource acquire = do
  (v0, u0) <- unsafeEff_ acquire
  var <- unsafeEff_ $ STM.newTMVarIO $ Value v0
  expiresAt <- unsafeEff_ $ STM.newTVarIO u0
  pure TimedResource {..}

readResource ::
  (Expiration :> es) =>
  TimedResource a ->
  Eff es a
readResource TimedResource {..} = do
  now <- unsafeEff_ UIO.getCurrentTime
  mres <- unsafeEff_ $ STM.atomically $ do
    timeLimit <- readTVar expiresAt
    if now < timeLimit
      then do
        rv <- tryTakeTMVar var
        case rv of
          Nothing -> retry
          Just UpdateFailed -> pure Nothing
          Just (Value a) -> do
            putTMVar var $ Value a
            pure $ Just a
      else Nothing <$ tryTakeTMVar var
  case mres of
    Just v -> pure v
    Nothing -> update `onException` unsafeEff_ (STM.atomically (tryPutTMVar var UpdateFailed))
  where
    update = unsafeEff_ $ do
      (new, limit) <- acquire
      STM.atomically $ do
        writeTVar expiresAt limit
        putTMVar var $ Value new
      pure new
