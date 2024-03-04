{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoFieldSelectors #-}

module Effectful.Network.Http (
  Http,
  Request (..),
  Response (..),
  runSimpleHttp,
  httpLbs,
  httpJSON,
  withResponse,
  withResponseStream,
  toGivesPopper,
  toRequestBody,
  toRequestBodyN,
  retryOn500,
  try404,
) where

import Control.Exception.Safe
import Data.Aeson (FromJSON)
import Data.Aeson qualified as A
import Data.Aeson.Parser qualified as AP
import Data.Attoparsec.ByteString qualified as A
import Data.Attoparsec.ByteString.Streaming qualified as AS
import Data.ByteString.Lazy qualified as LBS
import Data.Int (Int64)
import Data.Text qualified as T
import Effectful
import Effectful.Concurrent (Concurrent, threadDelay)
import Effectful.Dispatch.Dynamic
import Effectful.Dispatch.Static (unsafeEff_)
import Effectful.Log (Log, logAttention_)
import Effectful.Random.Static (Random, uniformR)
import Network.HTTP.Client (BodyReader, GivesPopper, HttpExceptionContent (..), RequestBody (..), setRequestCheckStatus)
import Network.HTTP.Client qualified as Http hiding (withResponse)
import Network.HTTP.Client.TLS (newTlsManager)
import Network.HTTP.Conduit (Request (..), Response (..))
import Network.HTTP.Simple (HttpException (..), addRequestHeader)
import Network.HTTP.Streaming (fromPopper)
import Network.HTTP.Streaming qualified as Raw
import Network.HTTP.Types (hAccept, notFound404, status500)
import Streaming.ByteString qualified as Q
import Text.Printf (printf)

runSimpleHttp :: (IOE :> es) => Eff (Http ': es) a -> Eff es a
runSimpleHttp act = do
  man <- newTlsManager
  flip interpret act $ \es -> \case
    ResponseClose rsp -> liftIO $ Http.responseClose rsp
    ResponseOpen req -> liftIO $ Http.responseOpen req man
    ToGivesPopper st -> localSeqUnliftIO es $ \unlift ->
      pure $ Raw.toGivesPopperWith unlift st

data Http :: Effect where
  ResponseOpen :: Request -> Http m (Response Http.BodyReader)
  ResponseClose :: Response a -> Http m ()
  ToGivesPopper :: Q.ByteStream m () -> Http m (GivesPopper ())

withResponse ::
  (HasCallStack, Http :> es) =>
  Request ->
  (Response BodyReader -> Eff es a) ->
  Eff es a
{-# INLINE withResponse #-}
withResponse req = bracket (responseOpen req) responseClose

responseOpen ::
  (HasCallStack, Http :> es) =>
  Request ->
  Eff es (Response Http.BodyReader)
{-# INLINE responseOpen #-}
responseOpen = send . ResponseOpen

responseClose :: (HasCallStack, Http :> es) => Response a -> Eff es ()
{-# INLINE responseClose #-}
responseClose = send . ResponseClose

type instance DispatchOf Http = 'Dynamic

toGivesPopper :: (Http :> es) => Q.ByteStream (Eff es) () -> Eff es (GivesPopper ())
toGivesPopper = send . ToGivesPopper

toRequestBody :: (Http :> es) => Q.ByteStream (Eff es) () -> Eff es RequestBody
toRequestBody = fmap RequestBodyStreamChunked . toGivesPopper

toRequestBodyN :: (Http :> es) => Int64 -> Q.ByteStream (Eff es) () -> Eff es RequestBody
toRequestBodyN sz = fmap (RequestBodyStream sz) . toGivesPopper

httpLbs :: (HasCallStack, Http :> es) => Request -> Eff es (Response LBS.ByteString)
httpLbs req = withResponse req $ \res -> do
  bss <- unsafeEff_ $ Http.brConsume $ responseBody res
  return res {responseBody = LBS.fromChunks bss}

withResponseStream ::
  (HasCallStack, Http :> es) =>
  Request ->
  (Response (Q.ByteStream IO ()) -> Eff es a) ->
  Eff es a
{-# INLINE withResponseStream #-}
withResponseStream req = withResponse req . (. fmap fromPopper)

httpJSON ::
  forall a es.
  (HasCallStack, FromJSON a, Http :> es) =>
  Request ->
  Eff es (Response a)
httpJSON req = withResponse
  ( setRequestCheckStatus $
      addRequestHeader hAccept "application/json" req
  )
  $ \rsp -> do
    (eith, _) <-
      unsafeEff_ $
        AS.parse
          (AP.json <* A.endOfInput)
          (fromPopper $ responseBody rsp)
    case eith of
      Left err -> throwM $ userError $ "Parse error: " <> show err
      Right v -> case A.fromJSON v of
        A.Success js -> pure rsp {responseBody = js}
        A.Error err -> throwM $ userError $ "Invalid JSON: " <> err

{- |
Returns a value if no exception occurred; returns 'Nothing' if 404 Not Found detected.
Otherwise throws the exception.
-}
retryOn500 :: (Concurrent :> es, Random :> es, Log :> es) => Eff es a -> Eff es a
retryOn500 act = loop 10 0.5e6
  where
    loop (!i :: Int) (!wait :: Double) = do
      r <- try act
      case r of
        Left (HttpExceptionRequest _ (StatusCodeException rsp _))
          | responseStatus rsp == status500
          , i > 0 -> do
              wait' <- uniformR (1e-6, wait)
              logAttention_ $
                "Storage Returned 500 ISE. Retrying after " <> T.pack (printf "%.06f" (wait' * 1e-6)) <> " seconds..."
              threadDelay $ floor wait'
              loop (i - 1) (wait * 1.5)
        Left e -> throwM e
        Right a -> pure a

{- |
Returns a value if no exception occurred; returns 'Nothing' if 404 Not Found detected.
Otherwise throws the exception.
-}
try404 :: (MonadCatch m) => m a -> m (Maybe a)
try404 act = do
  r <- try act
  case r of
    Left (HttpExceptionRequest _ (StatusCodeException rsp _))
      | responseStatus rsp == notFound404 -> pure Nothing
    Left e -> throwM e
    Right a -> pure $ Just a
