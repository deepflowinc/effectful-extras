{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}

module Effectful.Reader.Static.Lens (view) where

import qualified Control.Lens as Lens
import Data.Functor.Const (Const (..))
import Data.Profunctor.Unsafe ((#.))
import Effectful
import Effectful.Reader.Static

view :: (Reader s :> es) => Lens.Getting a s a -> Eff es a
view l = asks (getConst #. l Const)
