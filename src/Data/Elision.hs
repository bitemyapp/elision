{- |
Module      : Data.Elision
Description : Two functions with a missing "link" to be completed at a later time.
Copyright   : (c) 2016 Alex Crough
License     : MIT
Maintainer  : alex@crough.io
Stability   : Experimental
Portability : GADTs
-}
{-# LANGUAGE GADTs, NoImplicitPrelude #-}
module Data.Elision (Elision, completeM, complete, elide, (.?)) where

import Control.Applicative   (Applicative (..))
import Control.Category      (Category (..))
import Control.Monad         (Monad (..), (<=<))
import Data.Functor          (Functor (..))
import Data.Functor.Identity (Identity (..))
import Data.Profunctor       (Profunctor (..))

import Data.Function (const, flip)

data Elision s t a b where
  Complete :: (a -> b)                                         -> Elision s t a b
  Elided   :: (a -> s)               -> (t -> b)               -> Elision s t a b
  Composed :: Elision s t a c        -> Elision s t c b        -> Elision s t a b
  Applied  :: Elision s t a (x -> b) -> Elision s t a x        -> Elision s t a b
  Bound    :: Elision s t a c        -> (c -> Elision s t a b) -> Elision s t a b

instance Functor (Elision s t a) where
  fmap fn (Complete f)   = Complete (fn . f)
  fmap fn (Elided   f g) = Elided   f               (fn . g)
  fmap fn (Composed x y) = Composed x               (fmap fn y)
  fmap fn (Applied  x y) = Applied  (fmap (fn .) x) y
  fmap fn (Bound    x f) = Bound    x               (fmap fn . f)

instance Profunctor (Elision s t) where
  dimap l r (Complete f)   = Complete (r . f . l)
  dimap l r (Elided   f g) = Elided   (f . l)           (r . g)
  dimap l r (Composed y x) = Composed (lmap l y)        (rmap r x)
  dimap l r (Applied  f x) = Applied  (dimap l (r .) f) (lmap l x)
  dimap l r (Bound    x f) = Bound    (lmap l x)        (fmap (dimap l r) f)

instance Applicative (Elision s t a) where
  pure x = Complete (const x)
  (<*>)  = Applied

instance Monad (Elision s t a) where
  (>>=)  = Bound

instance Category (Elision s t) where
  id  = Complete id
  (.) = flip Composed

elide :: (a -> s) -> (t -> b) -> Elision s t a b
elide = Elided

(.?) :: (t -> b) -> (a -> s) -> Elision s t a b
(.?) = flip Elided

completeM :: Monad m => (s -> m t) -> Elision s t a b -> a -> m b
completeM _  (Complete f  ) = pure . f
completeM fn (Elided   f g) = fmap g . fn . f
completeM fn (Composed x y) = completeM fn y <=< completeM fn x
completeM fn (Applied  f x) = \arg -> completeM fn f arg <*> completeM fn x arg
completeM fn (Bound    x f) = \arg -> do x' <- completeM fn x arg
                                         completeM fn (f x') arg

complete :: (s -> t) -> Elision s t a b -> a -> b
complete fn x = runIdentity . completeM (pure . fn) x
