{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Effectful.Network.GitHub.Apps (
  -- * API Calls
  GitHub,
  GitHubConfig (..),
  AppID (..),
  CommitHash (..),
  runGitHubWith,
  runGitHubWith_,
  GitHubException (..),

  -- ** GitHub Repository Context
  GitHubRepo,
  Repository (..),
  parseRepo,
  withGitHubRepo,

  -- ** High-level bindings
  getRawContent,
  commentIssue,
  createBlob,
  BlobResult (..),
  createTree,
  TreeResult (..),
  SHA,
  fromBlobPaths,
  createTreeFromDirRecur,
  TreeEntry (..),
  TreeEntryType (..),
  GitEntryMode (..),
  GitTree (..),
  createCommit,
  NewCommit (..),
  CommitObj (..),
  CommitTree (..),
  getCommitObj,
  updateRefs,
  getGitRef,
  RefsResult (..),
  GitObject (..),
  getPull,
  Pull (..),

  -- *** Auxiliary function
  modeForEntry,

  -- ** Low-level operators
  callEndpointJSON,
  callEndpointLbs,
  parseRawAPIRequest,
  parseRawRepoAPIRequest,
  callGitHubAPI,

  -- * Tokens
  APITokens (..),
  APITokenConfig (..),
  GitHubAppToken (..),
  GitHubRepoTokens (..),
  newTimedAppToken,
  newTimedRepoTokens,
  newAPITokens,
  askRepoSetting,
  askRepoToken,

  -- * Re-exports
  GHEndpoint (..),
  queryGitHub,
  queryGitHub_,
  KeyValue (..),
  StdMethod (..),
) where

import Control.DeepSeq (NFData)
import Control.Exception (Exception)
import Control.Exception.Safe (throwM)
import Control.Monad (guard)
import Data.Aeson (FromJSON, ToJSON, (.=))
import Data.Aeson qualified as J
import Data.ByteString.Base64.Lazy qualified as B64
import Data.ByteString.Lazy qualified as LBS
import Data.Char qualified as C
import Data.Fix
import Data.Functor ((<&>))
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HM
import Data.Hashable (Hashable)
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NE
import Data.Maybe (fromJust, fromMaybe, isJust)
import Data.String (IsString, fromString)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import Data.Text.Lazy.Encoding qualified as LT
import Data.Time (ZonedTime, addUTCTime, zonedTimeToUTC)
import Data.Time.Clock (getCurrentTime)
import Data.Traversable (forM)
import Effectful
import Effectful.Concurrent.TimedResource (Expiration, TimedResource, newTimedResource, readResource)
import Effectful.Dispatch.Dynamic (HasCallStack, interpret, send)
import Effectful.Dispatch.Static (SideEffects (..), getStaticRep, unsafeEff_)
import Effectful.FileSystem (FileSystem, getFileSize)
import Effectful.FileSystem.Tagged (listDir, makeAbsolute, readFileBinaryLazy)
import Effectful.Internal.Monad (StaticRep, evalStaticRep)
import Effectful.Log (Log, logTrace)
import Effectful.Network.Http
import GHC.Generics (Generic)
import GHC.OldList qualified as L
import GitHub.REST (GHEndpoint (..), GitHubSettings (..), GitHubT, KeyValue (..), StdMethod (..), Token (..), queryGitHub, queryGitHubAll, queryGitHub_, runGitHubT)
import GitHub.REST.Auth (getJWTToken)
import Network.HTTP.Client (responseTimeoutNone)
import Path.Tagged
import Path.Tagged.IO (makeRelative)
import Web.JWT (EncodeSigner)

type instance DispatchOf GitHub = 'Dynamic

newtype CommitHash = CommitHash {hash :: Text}
  deriving (Show, Eq, Ord, Generic)
  deriving newtype (IsString, ToJSON, FromJSON, NFData, Hashable)

parseRepo :: String -> Maybe Repository
parseRepo str = do
  let (owner, T.drop 1 -> name) = T.breakOn "/" $ T.pack str
  guard $ not $ T.null name
  pure Repository {..}

data Repository = Repository {owner, name :: !T.Text}
  deriving (Show, Eq, Ord, Generic)
  deriving anyclass (Hashable, NFData)

instance ToJSON Repository where
  toJSON Repository {..} = J.toJSON $ owner <> "/" <> name

instance FromJSON Repository where
  parseJSON = J.withText "OWNER/NAME" \txt -> do
    let (owner, T.drop 1 -> name) = T.breakOn "/" txt
    guard $ not (T.null name) && not (T.null owner)
    pure Repository {..}

data GitHub :: Effect where
  HasRepo :: Repository -> GitHub m Bool
  LiftGitHubT :: Repository -> GitHubT IO a -> GitHub m a
  MkRawHttpReq :: Repository -> String -> GitHub m Request
  CallEndpointJSON :: (FromJSON a) => Request -> GitHub m (Response a)
  CallEndpointLbs :: Request -> GitHub m (Response LBS.ByteString)

data GitHubRepo :: Effect

type instance DispatchOf GitHubRepo = 'Static 'NoSideEffects

newtype instance StaticRep GitHubRepo = GHRepo Repository

hasRepo :: (GitHub :> es) => Repository -> Eff es Bool
hasRepo = send . HasRepo

callEndpointJSON ::
  (HasCallStack, FromJSON a, GitHub :> es) =>
  Request ->
  Eff es (Response a)
{-# INLINE callEndpointJSON #-}
callEndpointJSON = send . CallEndpointJSON

callEndpointLbs ::
  (HasCallStack, GitHub :> es) =>
  Request ->
  Eff es (Response LBS.ByteString)
{-# INLINE callEndpointLbs #-}
callEndpointLbs = send . CallEndpointLbs

runGitHubWith_ ::
  ( Http :> es
  , Expiration :> es
  ) =>
  APITokenConfig ->
  Eff (GitHub ': es) a ->
  Eff es a
runGitHubWith_ cfg act = do
  toks <- newAPITokens cfg
  runGitHubWith toks act

newtype GitHubException = UnknownRepo Repository
  deriving (Show, Eq, Ord, Generic)
  deriving anyclass (Exception)

runGitHubWith ::
  ( Http :> es
  , Expiration :> es
  ) =>
  APITokens ->
  Eff (GitHub ': es) a ->
  Eff es a
runGitHubWith tok = do
  interpret $ \_env -> \case
    HasRepo repo -> isJust <$> askRepoToken repo tok
    LiftGitHubT repo act -> do
      gh <-
        maybe (throwM $ UnknownRepo repo) pure
          =<< askRepoSetting tok.config tok repo
      unsafeEff_ $ runGitHubT gh act
    CallEndpointJSON req -> httpJSON req
    CallEndpointLbs req -> httpLbs req
    MkRawHttpReq repo endpoint -> do
      btok <-
        maybe (throwM $ UnknownRepo repo) pure
          =<< askRepoToken repo tok
      rawHttpReqImpl tok.config btok endpoint

parseRawRepoAPIRequest ::
  (HasCallStack, GitHub :> es, GitHubRepo :> es) =>
  String ->
  Eff es Request
parseRawRepoAPIRequest endpoint = do
  GHRepo repo <- getStaticRep
  let req =
        "/repos/"
          <> T.unpack repo.owner
          <> "/"
          <> T.unpack repo.name
          <> "/"
          <> endpoint
  parseRawAPIRequest req

rawHttpReqImpl ::
  APITokenConfig ->
  Token ->
  String ->
  Eff es Request
rawHttpReqImpl cfg btok endpoint = do
  let tok = case btok of
        AccessToken atk -> atk
        BearerToken bk -> bk
      req =
        (fromString $ "https://api.github.com" <> endpoint)
          { requestHeaders =
              [ ("Authorization", "Bearer " <> tok)
              , ("User-Agent", T.encodeUtf8 cfg.github.appName)
              ]
          }
  pure req

getRawContent ::
  ( GitHub :> es
  , HasCallStack
  , GitHubRepo :> es
  ) =>
  CommitHash ->
  FilePath ->
  Eff es LBS.ByteString
getRawContent commit fp = do
  req <-
    parseRawRepoAPIRequest $
      "contents/" <> fp <> "?ref=" <> T.unpack commit.hash
  responseBody <$> callEndpointLbs (req {requestHeaders = ("Accept", "application/vnd.github.raw") : requestHeaders req})

data Pull = Pull
  { url :: String
  , id :: Int
  , html_url :: Text
  , number :: Int
  , title :: Maybe Text
  , body :: Maybe Text
  }
  deriving (Show, Eq, Ord, Generic)
  deriving anyclass (FromJSON, ToJSON)

getPull ::
  (HasCallStack, GitHub :> es, GitHubRepo :> es) =>
  Repository ->
  Int ->
  Eff es Pull
getPull repo pull =
  callGitHubAPI $
    queryGitHub
      GHEndpoint
        { method = GET
        , ghData = []
        , endpointVals = ["owner" := repo.owner, "repo" := repo.name, "pull" := pull]
        , endpoint = "/repos/:owner/:repo/pulls/:pull"
        }

callGitHubAPI ::
  ( GitHub :> es
  , GitHubRepo :> es
  , HasCallStack
  ) =>
  GitHubT IO a ->
  Eff es a
callGitHubAPI act = do
  GHRepo repo <- getStaticRep
  send $ LiftGitHubT repo act

withGitHubRepo ::
  (GitHub :> es) => Repository -> Eff (GitHubRepo ': es) a -> Eff es a
withGitHubRepo repo act = do
  has <- hasRepo repo
  if has
    then evalStaticRep (GHRepo repo) act
    else throwM $ UnknownRepo repo

data BlobResult = BlobResult {url :: Text, sha :: Text}
  deriving (Show, Eq, Ord, Generic)
  deriving anyclass (FromJSON, ToJSON)

parseRawAPIRequest ::
  (HasCallStack, GitHub :> es, GitHubRepo :> es) =>
  String ->
  Eff es Request
{-# INLINE parseRawAPIRequest #-}
parseRawAPIRequest uri = do
  GHRepo repo <- getStaticRep
  send $ MkRawHttpReq repo uri

createBlob ::
  ( GitHub :> es
  , HasCallStack
  , GitHubRepo :> es
  ) =>
  LBS.ByteString ->
  Eff es BlobResult
createBlob src = do
  req0 <- parseRawRepoAPIRequest "git/blobs"
  let req =
        req0
          { method = "POST"
          , requestBody =
              RequestBodyLBS $
                J.encode $
                  J.object
                    [ "content" .= LT.decodeUtf8 (B64.encode src)
                    , "encoding" .= ("base64" :: Text)
                    ]
          , requestHeaders =
              ("Accept", "application/vnd.github+json")
                : filter ((/= "Accept") . fst) (requestHeaders req0)
          , responseTimeout = responseTimeoutNone
          }
  callEndpointLbs req >>= \rsp ->
    case J.eitherDecode $ responseBody rsp of
      Right v -> pure v
      Left err -> throwM $ userError $ err <> "\n" <> show (req, rsp)

type SHA = Text

data TreeEntry = TreeEntry
  { path :: FilePath
  , type_ :: TreeEntryType
  , mode :: GitEntryMode
  , sha :: SHA
  }
  deriving (Show, Eq, Ord, Generic)

newtype GitEntryMode = GitEntryMode {getPermission :: Text}
  deriving (Show, Eq, Ord, Generic)
  deriving newtype (IsString, FromJSON, ToJSON, Hashable, NFData)

data TreeEntryType = Blob | Tree
  deriving (Show, Eq, Ord, Generic)

modeForEntry :: TreeEntryType -> GitEntryMode
modeForEntry Blob = "100644"
modeForEntry Tree = "040000"

treeEntryTypeOpt :: J.Options
treeEntryTypeOpt =
  J.defaultOptions
    { J.allNullaryToStringTag = True
    , J.constructorTagModifier = map C.toLower
    }

instance FromJSON TreeEntryType where
  parseJSON = J.genericParseJSON treeEntryTypeOpt

instance ToJSON TreeEntryType where
  toJSON = J.genericToJSON treeEntryTypeOpt

treeEntryOpt :: J.Options
treeEntryOpt =
  J.defaultOptions
    { J.unwrapUnaryRecords = True
    , J.fieldLabelModifier =
        fromMaybe
          <$> id
          <*> fmap T.unpack . T.stripSuffix "_" . T.pack
    }

instance ToJSON TreeEntry where
  toJSON = J.genericToJSON treeEntryOpt

data TreeResult = TreeResult {url :: Text, sha :: SHA}
  deriving (Show, Eq, Ord, Generic)
  deriving anyclass (FromJSON, ToJSON)

data GitTree = GitTree {tree :: [TreeEntry], base_tree :: Maybe SHA}
  deriving (Show, Eq, Ord, Generic)
  deriving anyclass (ToJSON)

createTree ::
  ( GitHub :> es
  , HasCallStack
  , GitHubRepo :> es
  ) =>
  GitTree ->
  Eff es TreeResult
createTree tree = do
  req0 <- parseRawRepoAPIRequest "git/trees"
  let req =
        req0
          { method = "POST"
          , requestBody = RequestBodyLBS $ J.encode tree
          }
  responseBody <$> callEndpointJSON req

data GitTreeF a
  = DirEntry' (PathTo Unknown (RelTo Focused) Dir) [a]
  | FileEntry' (PathTo Unknown (RelTo Focused) File)
  deriving (Show, Eq, Ord, Functor, Foldable, Traversable)

data Focused

createTreeFromDirRecur ::
  ( FileSystem :> es
  , GitHub :> es
  , Log :> es
  , HasCallStack
  , GitHubRepo :> es
  ) =>
  Maybe SHA ->
  PathTo e b Dir ->
  Eff es TreeEntry
createTreeFromDirRecur mbaseSHA base0 = do
  base <- retagPath <$> makeAbsolute base0
  refoldM (toTreeEntry mbaseSHA base) (walkDownDir base) (Left [reldir|./|])

walkDownDir ::
  (HasCallStack, FileSystem :> es) =>
  PathTo Focused Abs Dir ->
  Either
    (PathTo Unknown (RelTo Focused) Dir)
    (PathTo Unknown (RelTo Focused) File) ->
  Eff
    es
    ( GitTreeF
        ( Either
            (PathTo Unknown (RelTo Focused) Dir)
            (PathTo Unknown (RelTo Focused) File)
        )
    )
walkDownDir root (Left target) = do
  let name = root </> target
  (dirs, files) <- listDir name
  pure $
    DirEntry' target $
      map (Left . fromJust . makeRelative root) dirs
        <> map (Right . fromJust . makeRelative root) files
walkDownDir _ (Right target) = do
  pure $ FileEntry' target

toTreeEntry ::
  ( FileSystem :> es
  , GitHub :> es
  , Log :> es
  , HasCallStack
  , GitHubRepo :> es
  ) =>
  Maybe SHA ->
  PathTo Focused Abs Dir ->
  GitTreeF TreeEntry ->
  Eff es TreeEntry
toTreeEntry baseSHA _ (DirEntry' pth children) = do
  let treeObj =
        GitTree
          { base_tree = baseSHA <* guard (pth == [reldir|./|])
          , tree = children
          }
  logTrace "Uploading DirEntry: " $
    J.object ["path" .= pth, "children" .= children, "payload" .= treeObj]
  tr@TreeResult {..} <- createTree treeObj
  logTrace "Tree Created" tr
  pure
    TreeEntry
      { sha
      , type_ = Tree
      , mode = modeForEntry Tree
      , path = init $ fromRelDir $ dirname pth
      }
toTreeEntry _ root (FileEntry' pth) = do
  size <- getFileSize $ fromAbsFile $ root </> pth
  logTrace "Uploading FileEntry'" $ J.object ["path" .= pth, "size" .= size]
  blb@BlobResult {..} <- createBlob =<< readFileBinaryLazy (root </> pth)
  logTrace "Blob created" blb
  pure $
    TreeEntry
      { path = fromRelFile $ filename pth
      , type_ = Blob
      , mode = modeForEntry Blob
      , sha
      }

data RefsResult = RefsResult
  { ref :: Text
  , node_id :: Text
  , url :: Text
  , object :: GitObject
  }
  deriving (Show, Eq, Ord, Generic)
  deriving anyclass (FromJSON, ToJSON)

data GitObject = GitObject
  { url :: Text
  , sha :: CommitHash
  }
  deriving (Show, Eq, Ord, Generic)
  deriving anyclass (FromJSON, ToJSON)

getGitRef ::
  ( GitHub :> es
  , HasCallStack
  , GitHubRepo :> es
  ) =>
  Text ->
  Eff es RefsResult
getGitRef ref =
  fmap responseBody . callEndpointJSON
    =<< parseRawRepoAPIRequest ("git/ref/" <> T.unpack ref)

data CommitObj = CommitObj {sha :: CommitHash, tree :: CommitTree}
  deriving (Show, Eq, Ord, Generic)
  deriving anyclass (FromJSON, ToJSON)

data CommitTree = CommitTree {sha :: SHA, url :: String}
  deriving (Show, Eq, Ord, Generic)
  deriving anyclass (FromJSON, ToJSON)

data NewCommit = NewCommit {message :: Text, tree :: SHA, parents :: [CommitHash]}
  deriving (Show, Eq, Ord, Generic)
  deriving anyclass (FromJSON, ToJSON)

createCommit ::
  ( GitHub :> es
  , HasCallStack
  , GitHubRepo :> es
  ) =>
  NewCommit ->
  Eff es CommitObj
createCommit comm = do
  req0 <- parseRawRepoAPIRequest "git/commits"
  let req = req0 {method = "POST", requestBody = RequestBodyLBS $ J.encode comm}
  responseBody <$> callEndpointJSON req

updateRefs ::
  ( GitHub :> es
  , HasCallStack
  , GitHubRepo :> es
  ) =>
  Text ->
  CommitHash ->
  Eff es J.Value
updateRefs ref commit = do
  req0 <- parseRawRepoAPIRequest $ "git/refs/" <> T.unpack ref
  let req = req0 {method = "PATCH", requestBody = RequestBodyLBS $ J.encode $ J.object ["sha" J..= commit]}
  fmap responseBody . callEndpointJSON $ req

decodeToken :: T.Text -> Token
decodeToken = BearerToken . T.encodeUtf8

newtype GitHubAppToken = GitHubAppToken {appToken :: Token}
  deriving (Show, Generic)

newtype GitHubRepoTokens = GitHubRepoTokens {repoTokens :: HashMap Repository (TimedResource Token)}
  deriving (Generic)

data APITokens = APITokens
  { app :: {-# UNPACK #-} !(TimedResource GitHubAppToken)
  , repos :: !GitHubRepoTokens
  , config :: !APITokenConfig
  }
  deriving (Generic)

newAPITokens ::
  (Expiration :> es) =>
  APITokenConfig ->
  Eff es APITokens
newAPITokens config = do
  app <- newTimedAppToken config
  repos <- newTimedRepoTokens config
  pure APITokens {..}

askRepoSetting ::
  ( Expiration :> es
  ) =>
  APITokenConfig ->
  APITokens ->
  Repository ->
  Eff es (Maybe GitHubSettings)
askRepoSetting APITokenConfig {..} toks repo = do
  askRepoToken repo toks <&> fmap \tok ->
    GitHubSettings
      { apiVersion = "2022-11-28"
      , token = Just tok
      , userAgent = T.encodeUtf8 github.appName
      }

askRepoToken ::
  (Expiration :> es) =>
  Repository ->
  APITokens ->
  Eff es (Maybe Token)
askRepoToken repo tok = do
  mapM readResource $ HM.lookup repo tok.repos.repoTokens

data APITokenConfig = APITokenConfig
  { github :: {-# UNPACK #-} !GitHubConfig
  , repos :: !(NonEmpty Repository)
  }
  deriving (Generic)

newTimedAppToken ::
  (Expiration :> es) =>
  APITokenConfig ->
  Eff es (TimedResource GitHubAppToken)
newTimedAppToken cfg = do
  newTimedResource $ do
    now <- getCurrentTime
    tok <- liftIO $ getJWTToken cfg.github.privKey cfg.github.appID.getAppID
    pure (GitHubAppToken tok, 600 `addUTCTime` now)

data Installation = Installation {id, target_id :: !Int, account :: !InstallationAccount}
  deriving (Show, Eq, Ord, Generic)
  deriving anyclass (J.FromJSON, J.ToJSON)

newtype InstallationAccount = InstAccount {login :: T.Text}
  deriving (Show, Eq, Ord, Generic)
  deriving anyclass (J.FromJSON, J.ToJSON)

data AccessTokenResult = AccessTokenResult
  { expires_at :: ZonedTime
  , token :: T.Text
  , repositories :: Maybe J.Value
  , repository_selection :: Maybe T.Text
  }
  deriving (Show, Generic)
  deriving anyclass (J.FromJSON, J.ToJSON)

newTimedRepoTokens ::
  ( Expiration :> es
  ) =>
  APITokenConfig ->
  Eff es GitHubRepoTokens
newTimedRepoTokens cfg = do
  repoToks <- forM cfg.repos $ \repo -> fmap (repo,) $ newTimedResource $ do
    -- NOTE: getJWTToken は 10 分間のみ有効な App トークンを生成する。
    -- このため、レポジトリ用のトークンを生成する直前に、個別に App トークンを生成する必要があり、
    -- この @appToken@ の生成を 'newTimedResource' の外側に移動してはならない (MUST NOT)。
    appTok <- getJWTToken cfg.github.privKey cfg.github.appID.getAppID
    let sett =
          GitHubSettings
            { apiVersion = "2022-11-28"
            , token = Just appTok
            , userAgent = T.encodeUtf8 cfg.github.appName
            }
    AccessTokenResult {..} <- runGitHubT sett $ do
      Just Installation {id = installId} <-
        L.find (\inst -> inst.account.login == repo.owner)
          <$> queryGitHubAll
            GHEndpoint
              { ghData = []
              , endpointVals = []
              , endpoint = "/app/installations"
              , method = GET
              }
      queryGitHub
        GHEndpoint
          { ghData = []
          , endpointVals = ["id" := installId]
          , endpoint = "/app/installations/:id/access_tokens"
          , method = POST
          }
    pure (decodeToken token, zonedTimeToUTC expires_at)
  pure $ GitHubRepoTokens $ HM.fromList $ NE.toList repoToks

data GitHubConfig = GitHubConfig
  { appName :: !T.Text
  , appID :: !AppID
  , privKey :: !EncodeSigner
  }
  deriving (Generic)

newtype AppID = AppID {getAppID :: Int}
  deriving (Eq, Ord, Generic)
  deriving newtype (Show, FromJSON, ToJSON)
  deriving newtype (Num, Real, Enum, Integral)

commentIssue ::
  ( GitHub :> es
  , HasCallStack
  , GitHubRepo :> es
  ) =>
  Int ->
  Text ->
  Eff es ()
commentIssue issue text = do
  GHRepo repo <- getStaticRep
  callGitHubAPI $
    queryGitHub_
      GHEndpoint
        { method = POST
        , endpoint = "/repo/:org/:repo/issues/:pr/comments"
        , endpointVals = ["org" := repo.owner, "repo" := repo.name, "pr" := issue]
        , ghData = ["body" := text]
        }

fromBlobPaths ::
  (FileSystem :> es, GitHub :> es, GitHubRepo :> es) =>
  PathTo p Abs Dir ->
  [PathTo e (RelTo p) File] ->
  Eff es [TreeEntry]
fromBlobPaths base blobs = forM blobs $ \relPath -> do
  BlobResult {..} <- createBlob =<< readFileBinaryLazy (base </> relPath)
  pure
    TreeEntry
      { path = fromRelFile relPath
      , type_ = Blob
      , mode = modeForEntry Blob
      , sha
      }

getCommitObj ::
  ( GitHub :> es
  , GitHubRepo :> es
  , HasCallStack
  ) =>
  CommitHash ->
  Eff es CommitObj
getCommitObj ref =
  fmap responseBody . callEndpointJSON
    =<< parseRawRepoAPIRequest ("git/commits/" <> T.unpack ref.hash)
