{-# LANGUAGE RecordWildCards #-}
module Better.Data.Graph
  ( Graph
  , empty
  , insert
  , singleton
  , rawData
  , Edge(..)
  , addEdgeToGraph
  ) where

import Prelude hiding (lookup)

import Data.IntMap (IntMap)
import qualified Data.IntMap.Strict as IntMap

import Better.Data.Vertex

import Control.Monad.State

type GraphBackend t e = IntMap.IntMap (Vertex t e)

type Graph t e a = State (GraphBackend t e) a

-- | Empty graph
empty :: Graph a e ()
empty = put mempty

-- | Inserts a piece of data into a "Graph"
insert :: (Ord e) => t -> Graph t e IntMap.Key
insert x = do
  k <- nextKey
  let v = vertex x
  modify $ IntMap.insert k v
  return k

-- | Creates a new "Graph" from a piece of data.
singleton :: (Ord e) => t -> Graph t e IntMap.Key
singleton x = insert x

-- | Guarantees unique keys for items in the "Graph"
nextKey :: Graph t e IntMap.Key
-- nextKey = if null g then return 0 else succ . last . gets $ IntMap.keys
nextKey = do
  keys <- gets IntMap.keys
  if null keys
    then return 0
    else return . succ . last $ keys

addEdgeToGraph :: (Ord e) => Edge e -> Graph t e ()
addEdgeToGraph e@Edge{..} = modify ( IntMap.adjust (addEdgeOut e) begin
                                   . IntMap.adjust (addEdgeIn  e) end )

rawData :: Graph t e a -> GraphBackend t e
rawData g = execState g mempty
