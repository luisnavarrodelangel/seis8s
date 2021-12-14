module Sound.Seis8s.LayerState where

import Control.Monad.State
import Sound.Seis8s.Harmony

import Data.Maybe
import Data.Fixed
import Data.Function
import Data.List as List


--this is about what do I need to keep track of ? -- better to keep it empty for now

data LayerState = LayerState deriving (Show)

emptyLayerState = LayerState
-- thing :: State LayerState [Event]
-- thing = do
--   s <- get
--   return s
