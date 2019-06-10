module Main where

import qualified Better.Data.Graph as Graph
import Better.Data.Graph.State
import Better.Class.Select
import Better
import Better.Data.Graph.Rank

import qualified Data.IntMap.Strict as IntMap

import Control.Monad.State
import Control.Monad.Identity (runIdentity)

import System.Random

main :: IO ()
-- main = rawData my_dorms >> return ()
main = do
  -- print simple_graph
  print $ cressRank 5 $ simple_graph
  -- rawData my_dorms >>= print . cressRank 5

my_dorms :: GraphT String Better IO ()
my_dorms = do
  fromListUnconnected [ "Baker", "Burton-Conner", "East Campus"
                      , "MacGregor", "Masseeh", "McCormic"
                      , "New House (Chocolate)", "New House (French)", "New House (German)"
                      , "New House (Spanish)", "New House (Numbered)", "New House (iHouse)"
                      , "Next", "Random", "Simmons"
                      ]
  replicateM_ 50 chooseOne
  -- graph <- get
  -- liftIO $ print graph -- get >>= print doesn't work for nebulous reasons.

simple_graph :: Graph.Graph String Better
simple_graph = runIdentity . rawData $ do
  a <- insert "A"
  b <- insert "B"
  c <- insert "C"
  d <- insert "D"
  a `betterThan` b
  b `betterThan` c
  c `betterThan` d
  -- d `betterThan` a
