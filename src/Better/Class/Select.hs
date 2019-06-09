{-# LANGUAGE FlexibleInstances #-}
module Better.Class.Select
  ( randomItem
  ) where

import qualified Better.Data.Graph as Graph
import qualified Better.Data.Graph.State as StateGraph
import Better.Data.Vertex
import Control.Monad.State

import Prelude hiding (lookup)
import System.Random
import Data.Maybe (fromJust)

import Data.IntMap.Strict (keys, lookup)

instance Random ([a] -> a) where
  random g = (selector, g') where
    selector = \l -> l !! (fst $ randomR (0, length l - 1) g)
    (_, g') = next g
  randomR = const random

instance Random (Graph.Graph t e -> (Graph.Key, t)) where
  random g = (selector, g') where
    selector =
      \l ->
          let key = (fst $ random g) $ keys l
              payload = vertexInfo . fromJust . lookup key $ l
          in (key, payload)
    (_, g') = next g

  randomR = const random

instance Monad m => Random (StateGraph.GraphT t e m (StateGraph.Key, t)) where
  random g = (selector, g') where
    selector = get >>= return (fst $ random g)
    (_, g') = next g

  randomR = const random

randomItem :: StateGraph.GraphT t e IO (StateGraph.Key, t)
randomItem = get >>= liftIO . (<*>) randomIO . return
