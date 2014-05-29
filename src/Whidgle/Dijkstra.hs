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

import Data.Function
import Data.List
import qualified Data.Map as M
import Data.Maybe
import qualified Data.Set as S

-- hack to flip Maybe's Ord instance
newtype FlipOrd a = FlipOrd a
  deriving (Show, Eq)

instance Ord a => Ord (FlipOrd (Maybe a)) where
  compare (FlipOrd Nothing) (FlipOrd Nothing) = EQ
  compare (FlipOrd Nothing) _ = GT
  compare _ (FlipOrd Nothing) = LT
  compare (FlipOrd x) (FlipOrd y) = compare x y

data Dij a c = Dij
  { _distances :: M.Map a c
  , _previous :: M.Map a a
  , _nodes :: S.Set a
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
    , _nodes = enumerate graph source
    }
  where
  body = do
    ns <- use nodes
    unless (S.size ns == 0) $ do
      u <- popMd
      forM_ (graph u) $ \v -> do
        du <- use $ distances.at u

        let alt = fmap (+ dist u v) du
        dv <- use $ distances.at v
        when (((<) `on` FlipOrd) alt dv) $ do
          distances.at v .= alt
          previous.at v .= Just u

      body

  popMd = do
    ns <- use nodes
    ds <- use distances

    let it = minimumBy (compare `on` (\x -> FlipOrd (ds^.at x))) $ S.toList ns
    nodes %= S.delete it
    return it

-- finds the total distance to a goal
distanceDij :: (Ord a, Ord c, Num c)
  =>  a -- Goal (single target)
  -> Dij a c -- Dijkstra map
  -> Maybe c
distanceDij = iterateDij (sum . map fst)

-- finds a path to a goal
pathDij :: (Ord a, Ord c, Num c)
  =>  a -- Goal (single target)
  -> Dij a c -- Dijkstra map
  -> Maybe [a]
pathDij = iterateDij (map snd)

-- helper function to traverse a Dijkstra path
iterateDij :: (Ord a, Ord c, Num c)
  => ([(c, a)] -> d) -- function from list to single element
  -> a -- Goal (single target)
  -> Dij a c -- Dijkstra map
  -> Maybe d
iterateDij fold a d =
  case searchDij' a d of
    [x] -> Nothing
    lst -> Just (fold . tail . reverse $ lst) -- don't include first point
  where
  searchDij' a' d'@(Dij {_previous = ps, _distances = ds}) =
    (ds M.! a', a'):maybe [] (\x -> searchDij' x d') (ps^.at a')

enumerate :: (Show a, Ord a) => (a -> [a]) -> a -> S.Set a
enumerate gr source = execState (enumerate' source) S.empty
  where
  enumerate' source' =
    forM_ (gr source') $ \x -> do
      st <- get
      unless (x `S.member` st) $ do
        modify (S.insert x)
        enumerate' x
