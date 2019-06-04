module Main where

import qualified Better.Data.Graph as Graph

import Data.Function ((&))

main :: IO ()
main = print . Graph.rawData $ my_graph

my_graph :: Graph.Graph String String ()
my_graph = do
  ka <- Graph.insert "test"
  kb <- Graph.insert "test2"
  let e = Graph.Edge ka kb "Test Edge"
  Graph.addEdgeToGraph e
