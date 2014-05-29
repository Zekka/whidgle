{-# LANGUAGE FlexibleInstances #-}
{-
 - Whidgle.Comparisons
 -
 - Some utility functions explaining how to create monadic comparisons.
 -}
module Whidgle.Comparisons
( comparingM
) where

import Data.Monoid

import Whidgle.Types

-- Like `comparing`, but for monadic functions.
comparingM :: (Monad m, Ord a) => (b -> m a) -> b -> b -> m Ordering
comparingM f x y = do
  x' <- f x
  y' <- f y
  return (compare x' y')

-- We need this to use <> to deal with secondary conditions
instance Monoid m => Monoid (Whidgle m) where
  mempty = return mempty
  mappend x y = do
    v1 <- x
    v2 <- y
    return (mappend v1 v2)
