{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}

module Effectful.State.Static.Shared.Lens (
  (.=),
  (%=),
  (?=),
  (<?=),
  (.@=),
  (%@=),
  (+=),
  (-=),
  (*=),
  (//=),
  (&&=),
  (||=),
  (<>=),
  (<.=),
  (<~),
  (%%=),
  (<%=),
  (<<%=),
  use,
) where

import Control.Lens hiding (use, (%%=), (%=), (%@=), (&&=), (*=), (+=), (-=), (.=), (.@=), (//=), (<%=), (<.=), (<<%=), (<>=), (<?=), (<~), (?=), (||=))
import qualified Control.Lens as Lens hiding (use)
import Data.Profunctor.Strong (Strong, second')
import Effectful
import Effectful.State.Static.Shared

(.=) :: (State s :> es) => ASetter s s a b -> b -> Eff es ()
{-# INLINE (.=) #-}
(.=) = fmap modify . (.~)

infix 4 .=

(?=) :: (State s :> es) => ASetter s s a (Maybe b) -> b -> Eff es ()
{-# INLINE (?=) #-}
(?=) = fmap modify . (?~)

infix 4 ?=

(<?=) :: (State s :> es) => ASetter s s a (Maybe b) -> b -> Eff es b
l <?= b = do
  l ?= b
  return b

infix 4 <?=

(%=) :: (State s :> es) => ASetter s s a b -> (a -> b) -> Eff es ()
{-# INLINE (%=) #-}
(%=) = fmap modify . (%~)

infix 4 %=

(+=) :: (State s :> es, Num a) => ASetter' s a -> a -> Eff es ()
{-# INLINE (+=) #-}
(+=) = fmap modify . (+~)

infix 4 +=

(-=) :: (State s :> es, Num a) => ASetter' s a -> a -> Eff es ()
{-# INLINE (-=) #-}
(-=) = fmap modify . (-~)

infix 4 -=

(*=) :: (State s :> es, Num a) => ASetter' s a -> a -> Eff es ()
{-# INLINE (*=) #-}
(*=) = fmap modify . (*~)

infix 4 *=

(//=) :: (State s :> es, Fractional a) => ASetter' s a -> a -> Eff es ()
{-# INLINE (//=) #-}
(//=) = fmap modify . (//~)

infix 4 //=

(&&=) :: (State s :> es) => ASetter' s Bool -> Bool -> Eff es ()
{-# INLINE (&&=) #-}
(&&=) = fmap modify . (&&~)

infix 4 &&=

(.@=) :: (State s :> es) => AnIndexedSetter i s s a b -> (i -> b) -> Eff es ()
{-# INLINE (.@=) #-}
(.@=) = fmap modify . (.@~)

infix 4 .@=

(%@=) :: (State s :> es) => AnIndexedSetter i s s a b -> (i -> a -> b) -> Eff es ()
{-# INLINE (%@=) #-}
(%@=) = fmap modify . (%@~)

infix 4 %@=

(||=) :: (State s :> es) => ASetter' s Bool -> Bool -> Eff es ()
{-# INLINE (||=) #-}
(||=) = fmap modify . (||~)

infix 4 ||=

(<>=) :: (State s :> es, Semigroup a) => ASetter' s a -> a -> Eff es ()
{-# INLINE (<>=) #-}
(<>=) = fmap modify . (<>~)

infix 4 <>=

(<~) :: (State s :> es) => ASetter s s a b -> Eff es b -> Eff es ()
l <~ mb = mb >>= (l .=)
{-# INLINE (<~) #-}

infixr 2 <~

(<.=) :: (State s :> es) => ASetter s s a b -> b -> Eff es b
{-# INLINE (<.=) #-}
l <.= b = do
  l .= b
  pure b

infix 4 <.=

(%%=) :: (State s :> es) => Over p ((,) r) s s a b -> p a (r, b) -> Eff es r
{-# INLINE (%%=) #-}
l %%= f = state (l f)

infix 4 %%=

(<%=) :: (State s :> es) => LensLike ((,) b) s s a b -> (a -> b) -> Eff es b
{-# INLINE (<%=) #-}
l <%= f = l %%= (\b -> (b, b)) . f

infix 4 <%=

(<<%=) :: (Strong p, State s :> es) => Over p ((,) a) s s a b -> p a b -> Eff es a
l <<%= f = l %%= lmap (\a -> (a, a)) (second' f)
{-# INLINE (<<%=) #-}

infix 4 <<%=

use :: (State s :> es) => Getting a s a -> Eff es a
{-# INLINE use #-}
use = gets . Lens.view
