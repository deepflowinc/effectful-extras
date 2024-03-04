{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoFieldSelectors #-}

module Effectful.Random.Static (Random, evalRandom, runRandom, uniformR) where

import Data.Coerce (coerce)
import Effectful
import Effectful.Concurrent.MVar.Strict (Concurrent, MVar, modifyMVar, newMVar, readMVar)
import Effectful.Dispatch.Static
import System.Random (StdGen)
import qualified System.Random as Rand

data Random :: Effect

type instance DispatchOf Random = 'Static 'NoSideEffects

newtype instance StaticRep Random = Random (MVar StdGen)

uniformR ::
  forall es a.
  ( Random :> es
  , Rand.UniformRange a
  , Concurrent :> es
  ) =>
  (a, a) ->
  Eff es a
uniformR ran = do
  Random mg <- getStaticRep @Random
  modifyMVar mg $ \g -> do
    let (a, g') = Rand.uniformR ran g
    pure (g', a)

runRandom :: forall es a. (Concurrent :> es) => StdGen -> Eff (Random ': es) a -> Eff es (a, StdGen)
{-# INLINE runRandom #-}
runRandom g act = do
  rep <- Random <$> newMVar g
  mapM (readMVar . coerce)
    =<< runStaticRep @Random @_ @es @a rep act

evalRandom :: (Concurrent :> es) => StdGen -> Eff (Random ': es) a -> Eff es a
{-# INLINE evalRandom #-}
evalRandom g act = do
  mg <- Random <$> newMVar g
  evalStaticRep mg act
