module Main where

import qualified Better.Data.Graph as Graph

main :: IO ()
main = print my_graph

my_graph :: Graph.Graph String String
my_graph = Graph.empty
  & Graph.insert "test"
  & Graph.insert "test2"

a & b = b a
