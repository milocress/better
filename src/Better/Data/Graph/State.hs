module Better.Data.Graph.State
  ( Graph
  , GraphT
  , empty
  , insert
  , Edge(..)
  , addEdgeToGraph
  , rawData
  , Key
  , fromPureGraph
  , fromListUnconnected
  ) where

import qualified Better.Data.Graph as Graph
import Better.Data.Vertex
import Control.Monad.Identity
import System.Random

-- Stateful frontend for Graph library

import Control.Monad.State

type GraphT t e m a = StateT (Graph.Graph t e) m a
type Graph t e a = GraphT t e Identity a
type Key = Graph.Key

empty :: (Monad m) => GraphT t e m ()
empty = put Graph.empty

insert :: (Ord e, Monad m) => t -> GraphT t e m Key
insert a = do
  k <- gets Graph.nextKey
  modify (Graph.insert a)
  return k

addEdgeToGraph :: (Ord e, Monad m) => Edge e -> GraphT t e m ()
addEdgeToGraph = modify . Graph.addEdgeToGraph

rawData :: (Monad m) => GraphT t e m a -> m (Graph.Graph t e)
rawData = flip execStateT mempty

fromPureGraph :: (Monad m) => Graph.Graph t e -> GraphT t e m ()
fromPureGraph = put

fromListUnconnected :: (Ord e, Monad m) => [t] -> GraphT t e m ()
fromListUnconnected = fromPureGraph . Graph.fromListUnconnected
