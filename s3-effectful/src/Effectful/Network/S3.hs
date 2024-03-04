{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module Effectful.Network.S3 (
  S3,
  runS3,
  BucketName,
  putObject,
  getObject,
  headObject,
  CopySource (..),
  copyObject,
  withObjectStream,
  deleteObject,
  listObjects,
  ListObjectsOpts (..),
  ObjectName,
  putBucket,
  getBucket,
  deleteBucket,
  -- Low-level combinators
  parseS3Request,
) where

import Control.Arrow ((>>>))
import Control.Exception.Safe (MonadThrow)
import Control.Lens ((^.), (^..))
import qualified Control.Lens as Lens
import Control.Monad
import qualified Data.ByteString.Lazy as LBS
import Data.Functor ((<&>))
import Data.Generics.Labels ()
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Effectful
import Effectful.Dispatch.Dynamic
import Effectful.Dispatch.Static (unsafeEff_)
import Effectful.Network.Http (Http, httpLbs, withResponseStream)
import GHC.Generics (Generic)
import NeatInterpolation (trimming)
import Network.HTTP.Client (Request, RequestBody (..), Response (..), requestFromURI, setRequestCheckStatus)
import Network.HTTP.Simple (addToRequestQueryString, setRequestBody, setRequestHeader, setRequestMethod)
import Network.HTTP.Types (ResponseHeaders, hAccept, hContentType)
import Network.Mime (defaultMimeLookup)
import Network.S3.Sign.V4 (S3Config (..), S3Endpoint (..), S3Region (..), signRequest)
import Network.URI (URI (..))
import Streaming (hoist)
import qualified Streaming.ByteString as Q
import qualified System.FilePath.Posix as PosFP
import qualified Text.XML.Hexml.Lens as X

type BucketName = T.Text

type ObjectName = T.Text

data CopySource = CopySource {bucket :: BucketName, object :: ObjectName}
  deriving (Show, Eq, Ord, Generic)

data S3 :: Effect where
  PutObject :: BucketName -> T.Text -> RequestBody -> S3 m ()
  CopyObject :: CopySource -> BucketName -> ObjectName -> S3 m ()
  GetObject :: BucketName -> T.Text -> S3 m LBS.ByteString
  HeadObject :: BucketName -> T.Text -> S3 m ResponseHeaders
  DeleteObject :: BucketName -> T.Text -> S3 m ()
  ListObjects :: ListObjectsOpts -> BucketName -> S3 m [ObjectName]
  WithObjectStream :: BucketName -> T.Text -> (Q.ByteStream m () -> m a) -> S3 m a
  PutBucket :: BucketName -> S3 m ()
  GetBucket :: BucketName -> S3 m LBS.ByteString
  DeleteBucket :: BucketName -> S3 m ()

newtype ListObjectsOpts = ListObjectsOpts
  { prefix :: Maybe T.Text
  }
  deriving (Show, Eq, Ord, Generic)

putObject :: (S3 :> es) => BucketName -> T.Text -> RequestBody -> Eff es ()
putObject = fmap (fmap send) . PutObject

putBucket :: (S3 :> es) => BucketName -> Eff es ()
putBucket = send . PutBucket

listObjects :: (S3 :> es) => ListObjectsOpts -> BucketName -> Eff es [ObjectName]
listObjects = fmap send . ListObjects

getBucket :: (S3 :> es) => BucketName -> Eff es LBS.ByteString
getBucket = send . GetBucket

deleteBucket :: (S3 :> es) => BucketName -> Eff es ()
{-# INLINE deleteBucket #-}
deleteBucket = send . DeleteBucket

withObjectStream :: (S3 :> es) => BucketName -> T.Text -> (Q.ByteStream (Eff es) () -> Eff es a) -> Eff es a
withObjectStream bucket path f = send $ WithObjectStream bucket path f

type instance DispatchOf S3 = 'Dynamic

deleteObject :: (S3 :> es) => BucketName -> T.Text -> Eff es ()
{-# INLINE deleteObject #-}
deleteObject = fmap send . DeleteObject

parseS3Request :: (MonadThrow m) => S3Endpoint -> String -> m Request
parseS3Request (S3Endpoint ep) path = do
  let rawPath =
        case uriPath ep of
          pth@('/' : _) -> pth
          pth -> '/' : pth
      uri = ep {uriPath = rawPath PosFP.</> path}
  requestFromURI uri

requestWith ::
  (Http :> es) =>
  (Request -> Request) ->
  S3Config ->
  FilePath ->
  Eff es (Response LBS.ByteString)
requestWith modif cfg@S3Config {..} path = do
  httpLbs
    =<< unsafeEff_
      . signRequest cfg
      . modif
      . setRequestCheckStatus
    =<< parseS3Request s3Endpoint path

signReq ::
  (Http :> es) =>
  S3Config ->
  Request ->
  Eff es Request
signReq = fmap unsafeEff_ . signRequest

putRequestWith ::
  (Http :> es) =>
  (Request -> Request) ->
  S3Config ->
  FilePath ->
  RequestBody ->
  Eff es (Response LBS.ByteString)
putRequestWith modif cfg p b =
  requestWith
    (modif . setRequestBody b . setRequestMethod "PUT")
    cfg
    p

getObject :: (S3 :> es) => BucketName -> T.Text -> Eff es LBS.ByteString
getObject = fmap send . GetObject

headObject :: (S3 :> es) => BucketName -> T.Text -> Eff es ResponseHeaders
headObject = fmap send . HeadObject

copyObject :: (S3 :> es) => CopySource -> BucketName -> ObjectName -> Eff es ()
copyObject = fmap (fmap send) . CopyObject

runS3 :: (Http :> es) => S3Config -> Eff (S3 ': es) a -> Eff es a
runS3 cfg = interpret $ \env -> \case
  PutObject bucket apath bdy -> do
    let mimeType =
          defaultMimeLookup $
            T.pack $
              PosFP.takeFileName $
                T.unpack apath
    void $
      putRequestWith
        (setRequestHeader hContentType [mimeType])
        cfg
        (T.unpack bucket PosFP.</> T.unpack apath)
        bdy
  CopyObject src bucket apath -> do
    void $
      putRequestWith
        ( setRequestHeader
            "x-amz-copy-source"
            [T.encodeUtf8 $ src.bucket <> "/" <> src.object]
        )
        cfg
        (T.unpack bucket PosFP.</> T.unpack apath)
        mempty
  GetObject bucket apath -> do
    req <- signReq cfg =<< getObjectRequest (cfg ^. #s3Endpoint) bucket apath
    responseBody <$> httpLbs req
  HeadObject bucket apath -> do
    req <- signReq cfg =<< headObjectRequest (cfg ^. #s3Endpoint) bucket apath
    responseHeaders <$> httpLbs req
  DeleteObject bucket apath ->
    void $
      requestWith
        (setRequestMethod "DELETE")
        cfg
        (T.unpack bucket PosFP.</> T.unpack apath)
  -- FIXME: HeXml は実態参照を解決しないし Strict BS 前提なのでシリアスに使うようになったら改善
  ListObjects opts bucket -> do
    lbs <-
      requestWith
        ( maybe
            id
            ( addToRequestQueryString
                . pure
                . ("prefix",)
                . Just
                . T.encodeUtf8
            )
            opts.prefix
            . setRequestMethod "GET"
        )
        cfg
        (T.unpack bucket)
    let objs =
          responseBody lbs
            ^.. X._XML
              . X.node ("ListBucketResult" :: LBS.ByteString)
              . X.node ("Contents" :: LBS.ByteString)
              . X.node ("Key" :: LBS.ByteString)
              . X._inner
              . Lens.to (T.decodeUtf8 . LBS.toStrict)
    pure objs
  WithObjectStream bucket apath k -> do
    req <- signReq cfg =<< getObjectRequest (cfg ^. #s3Endpoint) bucket apath
    withResponseStream req $ \rsp -> localSeqUnlift env $ \unlift ->
      unlift $ k $ hoist unsafeEff_ (responseBody rsp)
  PutBucket bucket -> do
    let S3Region (T.decodeUtf8 -> region) = cfg ^. #region
        bucketXml =
          [trimming|
              <?xml version="1.0" encoding="UTF-8"?>
              <CreateBucketConfiguration xmlns="http://s3.amazonaws.com/doc/2006-03-01/">
              <LocationConstraint>${region}</LocationConstraint>
              </CreateBucketConfiguration>
            |]
    void
      $ putRequestWith
        ( setRequestHeader "Content-Type" ["application/xml"]
            >>> setRequestHeader "x-amz-acl" ["private"]
        )
        cfg
        (T.unpack bucket)
      $ RequestBodyBS
      $ T.encodeUtf8 bucketXml
  GetBucket bucket -> do
    responseBody <$> requestWith (setRequestMethod "GET") cfg (T.unpack bucket)
  DeleteBucket bucket ->
    void $ requestWith (setRequestMethod "DELETE") cfg (T.unpack bucket)

{- HLINT ignore "Functor law" -}
getObjectRequest :: S3Endpoint -> BucketName -> T.Text -> Eff es Request
getObjectRequest ep bucket apath =
  parseS3Request
    ep
    (T.unpack bucket PosFP.</> T.unpack apath)
    <&> setRequestMethod "GET"
    <&> setRequestHeader hAccept ["application/octet-stream"]
    <&> setRequestCheckStatus

headObjectRequest :: S3Endpoint -> BucketName -> T.Text -> Eff es Request
headObjectRequest ep bucket apath =
  parseS3Request
    ep
    (T.unpack bucket PosFP.</> T.unpack apath)
    <&> setRequestMethod "HEAD"
    <&> setRequestHeader hAccept ["application/octet-stream"]
    <&> setRequestCheckStatus
