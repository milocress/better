{-# LANGUAGE DeriveFoldable #-}
module Better.Data.Graph
  ( Graph
  , empty
  , insert
  ) where

import Prelude hiding (lookup)

import Data.IntMap (IntMap)
import qualified Data.IntMap.Strict as IntMap

import qualified Data.Set as Set

type Graph a e = IntMap.IntMap (Vertex a e)

data Vertex a e = Vertex { edges      :: Set.Set (Edge e)
                         , vertexInfo :: a
                         } deriving Show

data Edge e = Edge { begin    :: Int
                   , end      :: Int
                   , edgeInfo :: Int
                   } deriving (Show, Eq, Ord)

empty :: Graph a e
empty = mempty

insert :: a -> Graph a e -> Graph a e
insert x g = IntMap.insert k v g where
  v = Vertex mempty x
  k = length g

