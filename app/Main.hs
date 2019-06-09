module Main where

import qualified Better.Data.Graph as Graph
import Better.Data.Graph.State
import Better.Class.Select
import Better

import qualified Data.IntMap.Strict as IntMap

import Control.Monad.State

import System.Random

main :: IO ()
main = rawData my_dorms >> return ()

my_dorms :: GraphT String Better IO ()
my_dorms = do
  fromListUnconnected ["Simmons", "Baker", "Random", "Macgregor", "New", "Next"]
  replicateM 10 chooseOne
  graph <- get
  liftIO $ print graph -- get >>= print doesn't work for nebulous reasons.

