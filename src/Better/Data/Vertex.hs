module Better.Data.Vertex
  ( Vertex ()
  , vertex
  , Edge (..)
  , addEdgeOut, addEdgeIn
  ) where

import qualified Data.Set as Set

data Vertex t e = Vertex { edges_out  :: Set.Set (Edge e)
                         , edges_in   :: Set.Set (Edge e)
                         , vertexInfo :: t
                         } deriving Show

data Edge e = Edge { begin    :: Int
                   , end      :: Int
                   , edgeInfo :: e
                   } deriving (Show, Eq, Ord)

vertex :: (Ord e) => t -> Vertex t e
vertex x = Vertex mempty mempty x

addEdgeOut, addEdgeIn :: (Ord e) => Edge e -> Vertex t e -> Vertex t e
addEdgeOut e v = v { edges_out = Set.insert e (edges_out v) }
addEdgeIn  e v = v { edges_in  = Set.insert e (edges_in  v) }
