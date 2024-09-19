{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module Effectful.Development.Shake (
  Shake,
  runShake,
  shake,
  shakeArgs,
  shakeOutputToLog,
  module Development.Shake,
) where

import Control.Monad (forM_)
import qualified Data.Aeson.Types as A
import Data.String (IsString (..))
import Development.Shake hiding (shake, shakeArgs)
import qualified Development.Shake as Raw
import Effectful
import Effectful.Concurrent (Concurrent)
import Effectful.Dispatch.Static
import Effectful.Environment (Environment)
import Effectful.FileSystem (FileSystem)
import Effectful.Log (Log, LogLevel (..), localDomain, logMessage)
import qualified Effectful.Process as P

-- TODO: Rules / Action to parametrised over effects

data Shake :: Effect

type instance DispatchOf Shake = Static 'WithSideEffects

data instance StaticRep Shake = ShakeRep

shakeOutputToLog :: (Log :> es) => ShakeOptions -> Eff es ShakeOptions
shakeOutputToLog opts = localDomain "shake" do
  unsafeEff \st -> do
    pure
      opts
        { shakeOutput = \lvl msg -> do
            let lvl' = case lvl of
                  Silent -> Nothing
                  Error -> Just LogAttention
                  Warn -> Just LogAttention
                  Info -> Just LogInfo
                  Verbose -> Just LogTrace
                  Diagnostic -> Just LogTrace
            forM_ lvl' \l ->
              unEff (logMessage l (fromString msg) A.emptyObject) st
        }

runShake ::
  ( IOE :> es
  , P.Process :> es
  , FileSystem :> es
  , Concurrent :> es
  , Environment :> es
  ) =>
  Eff (Shake ': es) a ->
  Eff es a
runShake = evalStaticRep ShakeRep

shake :: (Shake :> es) => ShakeOptions -> Rules () -> Eff es ()
shake = fmap unsafeEff_ . Raw.shake

shakeArgs :: (Shake :> es) => ShakeOptions -> Rules () -> Eff es ()
shakeArgs = fmap unsafeEff_ . Raw.shakeArgs
