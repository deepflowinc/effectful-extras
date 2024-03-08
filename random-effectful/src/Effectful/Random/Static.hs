{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoFieldSelectors #-}

module Effectful.Random.Static (
  Random,
  evalRandom,
  runRandom,
  split,

  -- * Combinators
  Rand.Uniform,
  uniform,
  Rand.UniformRange,
  uniformR,
  genByteString,

  -- ** Additional utilities
  element,

  -- * Re-exports
  RandomGen (),
  StdGen,
  mkStdGen,
) where

import qualified Data.ByteString as BS
import Data.Coerce (coerce)
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
import qualified Data.Vector as V
import Effectful
import Effectful.Concurrent.MVar.Strict (Concurrent, MVar, modifyMVar, newMVar, readMVar)
import Effectful.Dispatch.Static
import System.Random (RandomGen (), StdGen, mkStdGen)
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

uniform ::
  forall es a.
  ( Random :> es
  , Rand.Uniform a
  , Concurrent :> es
  ) =>
  Eff es a
uniform = do
  Random mg <- getStaticRep @Random
  modifyMVar mg $ \g -> do
    let (a, g') = Rand.uniform g
    pure (g', a)

runRandom :: forall es a. (Concurrent :> es) => StdGen -> Eff (Random ': es) a -> Eff es (a, StdGen)
{-# INLINE runRandom #-}
runRandom g act = do
  rep <- Random <$> newMVar g
  mapM (readMVar . coerce)
    =<< runStaticRep @Random @_ @es @a rep act

-- | Splits the current standard generator, updates it with one of the results,
split :: (Random :> es, Concurrent :> es) => Eff es StdGen
{-# INLINE split #-}
split = do
  Random mg <- getStaticRep @Random
  modifyMVar mg $ \g -> do
    let (g', g'') = Rand.split g
    pure (g'', g')

genByteString ::
  (Random :> es, Concurrent :> es) =>
  Int ->
  Eff es BS.ByteString
genByteString n = do
  Random mg <- getStaticRep @Random
  modifyMVar mg $ \g ->
    let (bs, g') = Rand.genByteString n g
     in pure (g', bs)

evalRandom :: (Concurrent :> es) => StdGen -> Eff (Random ': es) a -> Eff es a
{-# INLINE evalRandom #-}
evalRandom g act = do
  mg <- Random <$> newMVar g
  evalStaticRep mg act

element ::
  (Random :> es, Concurrent :> es) => NonEmpty a -> Eff es a
element els = do
  let v = V.fromList $ NE.toList els
  (v V.!) <$> uniformR (0, V.length v - 1)
