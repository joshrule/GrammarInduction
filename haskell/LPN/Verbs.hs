
module Verbs where

import Data.List
import System.Random.Shuffle (shuffle', shuffleM)
import Control.Monad.Random.Class
import Control.Monad


import LPN.Parse
import LPN.Prismify


ipaLexicon = do xs <- readFile "./experiments/prism/resources/englishPhones.txt" 
                return $ lines xs

