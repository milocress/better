module Better.Data.Vertex
  ( Vertex (..)
  , vertex
  , Edge (..)
  , addEdgeOut, addEdgeIn
  ) where

import qualified Data.Set as Set

data Vertex a e = Vertex { edges_out  :: Set.Set (Edge e)
                         , edges_in   :: Set.Set (Edge e)
                         , vertexInfo :: a
                         } deriving (Show)

data Edge e = Edge { begin    :: Int
                   , end      :: Int
                   , edgeInfo :: e
                   } deriving (Show, Eq, Ord)

vertex :: (Ord e) => a -> Vertex a e
vertex x = Vertex mempty mempty x

addEdgeOut, addEdgeIn :: (Ord e) => Edge e -> Vertex a e -> Vertex a e
addEdgeOut e v = v { edges_out = Set.insert e (edges_out v) }
addEdgeIn  e v = v { edges_in  = Set.insert e (edges_in  v) }

inDegree, outDegree :: (Ord e) => Vertex a e -> Int
inDegree  = Set.size . edges_in
outDegree = Set.size . edges_out
