module Better.Data.Graph.Rank where

-- Better ranking system.

import Better.Data.Graph
import Better.Data.Vertex
import Better

import Data.List (iterate, sortOn)

cressRank :: Int -> Graph a Better -> [(a, Double)]
cressRank n = sortOn snd
            . fmap vertexInfo
            . elems
            . (!! n)
            . iterate tick
            . toRankGraph

type RankGraph a = Graph (a, Double) Better
type RankVertex a = Vertex (a, Double) Better

defaultRank = 0.5

toRankGraph :: Graph a Better -> RankGraph a
toRankGraph = fmap (\v -> v { vertexInfo = (vertexInfo v, defaultRank) })

putRank :: Double -> RankVertex a -> RankVertex a
putRank n v = v { vertexInfo = (fst $ vertexInfo v, n) }

tick :: RankGraph a -> RankGraph a
tick g = mapWithKey f g where
  f = \k v ->
    let
      b =      inferiors g k
      t = b + (superiors g k)
      s = snd $ vertexInfo v
      r = (b + s) / (t + 1)
    in putRank r v

inferiors g k = sum $ fmap snd $ outVals g k
superiors g k = sum $ fmap ((1 - ) . snd) $ inVals  g k
