{-# LANGUAGE FlexibleInstances, TemplateHaskell, NoMonomorphismRestriction #-}
{-
 - Whidgle.Dijkstra
 -
 - A Dijkstra's algorithm implementation. Builds costs for the whole graph and then
 - lets you pick which node is a goal.
 -}
module Whidgle.Dijkstra
( buildDij, distanceDij, pathDij
) where

import Control.Lens
import Control.Monad.State hiding (forM)

import Data.Maybe
import qualified Data.Map as M
import qualified Data.PSQueue as PQ
import qualified Data.Set as S

-- this is basically another flavor of Maybe
data Infinite a = Only a | Infinity
  deriving (Eq, Ord, Show)

-- these rules are screwy, so try not to think about them too hard
-- (PLEASE do not try to pull out my Infinite type for general use!)
instance Num a => Num (Infinite a) where
  Infinity + _ = Infinity
  _ + Infinity = Infinity
  (Only x) + (Only y) = Only (x + y)

  Infinity * _ = Infinity
  _ * Infinity = Infinity
  (Only x) * (Only y) = Only (x * y)

  signum Infinity = 1
  signum (Only x) = Only (signum x)

  fromInteger x = Only (fromInteger x)

  abs Infinity = Infinity
  abs (Only x) = Only (abs x)

  negate Infinity = Infinity
  negate (Only x) = Only (-x)

unInfinite (Only a) = a
unInfinite _ = error "infinity"

m2I :: Maybe a -> Infinite a
m2I (Just x) = Only x
m2I Nothing = Infinity

i2M :: Infinite a -> Maybe a
i2M (Only x) = Just x
i2M (Infinity) = Nothing

data Dij a c = Dij
  { _distances :: M.Map a c
  , _previous :: M.Map a a
  , _nodes :: PQ.PSQ a (Infinite c)
  }
  deriving (Show)

makeLenses ''Dij

buildDij :: (Show a, Show c, Ord a, Ord c, Num c)
  => (a -> [a]) -- describes the graph
  -> (a -> a -> c) -- distance function
  -> a -- source
  -> Dij a c -- Dijkstra map
buildDij graph dist source =
  execState body Dij
    { _distances = M.fromList [(source, 0)]
    , _previous = M.empty
    , _nodes = heapify (enumerate graph source)
    }
  where
  heapify s =
    let basic = PQ.fromAscList [x PQ.:-> Infinity | x <- S.toList s] in
    PQ.adjust (const 0) source basic

  body = do
    ns <- use nodes
    unless (PQ.size ns == 0) $ do
      u <- popMin
      forM_ (graph u) $ \v -> do
        du <- use $ distances.at u.to m2I
        dv <- use $ distances.at v.to m2I
        let alt = (Only $ dist u v) + du

        when (alt < dv) $ do
          distances.at v .= i2M alt
          previous.at v .= Just u
          nodes %= PQ.adjust (const alt) v

      body

  popMin = do
    ns <- use nodes
    nodes %= PQ.deleteMin
    return (PQ.key . fromJust . PQ.findMin $ ns)

-- finds the total distance to a goal
distanceDij :: (Ord a, Ord c, Num c)
  =>  a -- Goal (single target)
  -> Dij a c -- Dijkstra map
  -> Maybe c
distanceDij = iterateDij (sum . map (unInfinite . fst))

-- finds a path to a goal
pathDij :: (Ord a, Ord c, Num c)
  =>  a -- Goal (single target)
  -> Dij a c -- Dijkstra map
  -> Maybe [a]
pathDij = iterateDij (map snd)

-- helper function to traverse a Dijkstra path
iterateDij :: (Ord a, Ord c, Num c)
  => ([(Infinite c, a)] -> d) -- function from list to single element
  -> a -- Goal (single target)
  -> Dij a c -- Dijkstra map
  -> Maybe d
iterateDij fold a d =
  case searchDij' a d of
    [_] -> Nothing
    lst -> Just (fold . tail . reverse $ lst) -- don't include first point
  where
  searchDij' a' d'@(Dij {_previous = ps, _distances = ds}) =
    (m2I (ds^.at a'), a'):maybe [] (\x -> searchDij' x d') (ps^.at a')

enumerate :: (Show a, Ord a) => (a -> [a]) -> a -> S.Set a
enumerate gr source = execState (enumerate' source) S.empty
  where
  enumerate' source' =
    forM_ (gr source') $ \x -> do
      st <- get
      unless (x `S.member` st) $ do
        modify (S.insert x)
        enumerate' x
