{-# LANGUAGE RecordWildCards #-}
module Better.Data.Graph
  ( Graph
  , empty
  , insert
  , Edge(..)
  , addEdgeToGraph
  , Key
  , nextKey
  , fromListUnconnected
  , inVals, outVals
  , IntMap.mapWithKey, IntMap.adjust, IntMap.elems
  ) where

import Prelude hiding (lookup)

import Data.IntMap (IntMap)
import qualified Data.IntMap.Strict as IntMap

import Better.Data.Vertex

-- import Control.Monad.State

type Graph a e = IntMap.IntMap (Vertex a e)
type Key = IntMap.Key

-- | Empty graph
empty :: Graph a e
empty = mempty

-- | Inserts a piece of data into a "Graph"
insert :: (Ord e) => a -> Graph a e -> Graph a e
insert x g = IntMap.insert k v g where
  k = nextKey g
  v = vertex x

-- | Guarantees unique keys for items in the "Graph"
nextKey :: Graph a e -> Key
-- nextKey = if null g then return 0 else succ . last . gets $ IntMap.keys
nextKey g = if null keys then 0
  else succ . last $ keys where
  keys = IntMap.keys g

addEdgeToGraph :: (Ord e) => Edge e -> Graph a e -> Graph a e
addEdgeToGraph e@Edge{..} = ( IntMap.adjust (addEdgeOut e) begin
                            . IntMap.adjust (addEdgeIn  e) end )

fromListUnconnected :: (Ord e) => [a] -> Graph a e
fromListUnconnected = foldr insert mempty

inVals, outVals :: Graph a e -> Key -> [a]
inVals  g k = vertexInfo <$> (g IntMap.!) <$> (inKeys  $ g IntMap.! k)
outVals g k = vertexInfo <$> (g IntMap.!) <$> (outKeys $ g IntMap.! k)
