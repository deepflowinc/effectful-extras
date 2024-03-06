{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Effectful.Network.GitHub.OAuth (
  GitHub (),
  runGitHubPublic,
  RW,
  RO,
  Error (..),
  GitHubException (..),
  github,
  Auth (..),
  runGitHub,
) where

import Control.Exception.Safe
import Effectful
import Effectful.Dispatch.Dynamic
import GHC.Generics (Generic)
import GitHub (Auth (..), Error (..))
import GitHub qualified as GH

type RO = 'GH.RO

type RW = 'GH.RW

newtype GitHubException = GitHubException Error
  deriving (Show, Generic)
  deriving anyclass (Exception)

data GitHub rw m a where
  GitHubReq :: (GH.ParseResponse media a) => GH.GenRequest media rw a -> GitHub rw m a

type instance DispatchOf (GitHub rw) = 'Dynamic

github ::
  forall rw a media es.
  (GH.ParseResponse media a, GitHub rw :> es) =>
  GH.GenRequest media rw a ->
  Eff es a
github = send . GitHubReq

runGitHubPublic ::
  (IOE :> es) => Eff (GitHub RO ': es) a -> Eff es a
runGitHubPublic = interpret \_ -> \case
  GitHubReq req -> do
    either (throwM . GitHubException) pure =<< liftIO (GH.github' req)

runGitHub ::
  (IOE :> es) => Auth -> Eff (GitHub RW ': es) a -> Eff es a
runGitHub auth = interpret \_ -> \case
  GitHubReq req -> do
    either (throwM . GitHubException) pure =<< liftIO (GH.github auth req)
