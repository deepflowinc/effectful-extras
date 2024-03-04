{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module Effectful.Time (
  Clock,
  runClock,
  getCurrentTime,
  getZonedTime,
  getCurrentTimeZone,
  UTCTime,
  TimeZone,
) where

import Data.Time (TimeZone, UTCTime, ZonedTime)
import qualified Data.Time as Time
import Effectful
import Effectful.Dispatch.Static

data Clock :: Effect

type instance DispatchOf Clock = 'Static 'WithSideEffects

data instance StaticRep Clock = TimeRep

getCurrentTimeZone :: Clock :> es => Eff es TimeZone
getCurrentTimeZone = unsafeEff_ Time.getCurrentTimeZone

getCurrentTime :: Clock :> es => Eff es UTCTime
getCurrentTime = unsafeEff_ Time.getCurrentTime

getZonedTime :: Clock :> es => Eff es ZonedTime
getZonedTime = unsafeEff_ Time.getZonedTime

runClock :: IOE :> es => Eff (Clock ': es) a -> Eff es a
runClock = evalStaticRep TimeRep
