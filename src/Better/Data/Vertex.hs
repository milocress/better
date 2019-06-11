module Better.Data.Vertex
  ( Vertex (..)
  , vertex
  , Edge (..)
  , addEdgeOut, addEdgeIn
  , outKeys, inKeys
  ) where

import qualified Data.Set as Set

type Key = Int

data Vertex a e = Vertex { edges_out  :: Set.Set (Edge e)
                         , edges_in   :: Set.Set (Edge e)
                         , vertexInfo :: a
                         } deriving (Show)

data Edge e = Edge { begin    :: Key
                   , end      :: Key
                   , edgeInfo :: e
                   } deriving (Show, Eq, Ord)

vertex :: (Ord e) => a -> Vertex a e
vertex x = Vertex mempty mempty x

addEdgeIn, addEdgeOut :: (Ord e) => Edge e -> Vertex a e -> Vertex a e
addEdgeIn  e v = v { edges_in  = Set.insert e (edges_in  v) }
addEdgeOut e v = v { edges_out = Set.insert e (edges_out v) }

inDegree, outDegree :: (Ord e) => Vertex a e -> Key
inDegree  = Set.size . edges_in
outDegree = Set.size . edges_out

inKeys, outKeys :: Vertex a e -> [Int]
inKeys  v = begin <$> (Set.toList . edges_in  $ v)
outKeys v = end   <$> (Set.toList . edges_out $ v)
