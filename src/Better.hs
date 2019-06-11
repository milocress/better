module Better where

import Better.Class.Select
import Better.Data.Graph.State

import Control.Monad.Trans

data Better = Better deriving (Show, Eq, Ord)

betterThan, worseThan :: (Monad m) => Key -> Key -> GraphT t Better m ()
betterThan ka kb = do
  addEdgeToGraph . Edge ka kb $ Better

worseThan = flip betterThan

chooseOne :: (Show t) => GraphT t Better IO ()
chooseOne = do
  (ka, sa) <- randomItem
  (kb, sb) <- randomItem
  if ka /= kb then do
    liftIO . putStrLn
      $ "Which one: " ++ (show sa) ++
               " or " ++ (show sb) ++ "? (a/b)"
    whichOne <- liftIO getLine
    case whichOne of
      "a" -> ka `betterThan` kb
      "b" -> ka `worseThan`  ka
      _   -> return () -- useful for when a user chooses not to make a comparison

  else
    chooseOne
