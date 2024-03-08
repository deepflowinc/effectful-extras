{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Effectful.Random.Static (
  Random,
  evalRandom,
  runRandom,
  split,

  -- ** Initialisation with global StdGen
  getStdGen,
  newStdGen,

  -- * Combinators
  Rand.Uniform,
  uniform,
  Rand.UniformRange,
  uniformR,
  genByteString,
  genShortByteString,
  withStdGen,

  -- ** Additional utilities
  element,

  -- ** Compat with 'Rand.StatefulGen'
  EffGen (..),
  FrozenEffGen (..),

  -- * Re-exports
  RandomGen (),
  StdGen,
  mkStdGen,
) where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Short as SBS
import Data.Coerce (coerce)
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
import qualified Data.Vector as V
import Effectful
import Effectful.Concurrent.MVar.Strict (Concurrent, MVar, modifyMVar, newMVar, readMVar, swapMVar)
import Effectful.Dispatch.Static
import GHC.Generics (Generic)
import System.Random (RandomGen (), StdGen, mkStdGen)
import qualified System.Random as Rand
import qualified System.Random.Stateful as RS

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

genShortByteString ::
  (Random :> es, Concurrent :> es) =>
  Int ->
  Eff es SBS.ShortByteString
{-# INLINE genShortByteString #-}
genShortByteString n = do
  Random mg <- getStaticRep @Random
  modifyMVar mg $ \g ->
    let (bs, g') = Rand.genShortByteString n g
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

getStdGen :: (MonadIO m) => m StdGen
getStdGen = liftIO Rand.getStdGen

newStdGen :: (MonadIO m) => m StdGen
newStdGen = liftIO Rand.newStdGen

data EffGen = EffGen
  deriving (Show, Eq, Ord, Generic)

withStdGen ::
  (Concurrent :> es, Random :> es) =>
  (StdGen -> (a, StdGen)) ->
  Eff es a
withStdGen f = do
  Random mg <- getStaticRep @Random
  modifyMVar mg $ \g ->
    let (a, g') = f g
     in pure (g', a)

instance (Concurrent :> es, Random :> es) => RS.StatefulGen EffGen (Eff es) where
  uniformWord32R = const . withStdGen . Rand.genWord32R
  {-# INLINE uniformWord32R #-}
  uniformWord64R = const . withStdGen . Rand.genWord64R
  {-# INLINE uniformWord64R #-}
  uniformWord8 = const $ withStdGen Rand.genWord8
  {-# INLINE uniformWord8 #-}
  uniformWord16 = const $ withStdGen Rand.genWord16
  {-# INLINE uniformWord16 #-}
  uniformWord32 = const $ withStdGen Rand.genWord32
  {-# INLINE uniformWord32 #-}
  uniformWord64 = const $ withStdGen Rand.genWord64
  {-# INLINE uniformWord64 #-}
  uniformShortByteString = const . genShortByteString
  {-# INLINE uniformShortByteString #-}

newtype FrozenEffGen = FrozenEffGen {getFrozenEffGen :: StdGen}
  deriving (Show, Eq, Generic)
  deriving newtype (RandomGen)

instance (Concurrent :> es, Random :> es) => RS.FrozenGen FrozenEffGen (Eff es) where
  type MutableGen FrozenEffGen (Eff es) = EffGen
  freezeGen = const $ do
    Random mg <- getStaticRep @Random
    FrozenEffGen <$> readMVar mg
  {-# INLINE freezeGen #-}
  thawGen (FrozenEffGen g) = do
    Random mg <- getStaticRep @Random
    EffGen <$ swapMVar mg g
  {-# INLINE thawGen #-}

instance
  (Concurrent :> es, Random :> es) =>
  RS.RandomGenM EffGen FrozenEffGen (Eff es)
  where
  applyRandomGenM :: forall a. (FrozenEffGen -> (a, FrozenEffGen)) -> EffGen -> Eff es a
  applyRandomGenM = const . coerce (withStdGen @es @a)
  {-# INLINE applyRandomGenM #-}
