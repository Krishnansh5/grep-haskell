module Regex where

import qualified Data.Text as T
import Data.Text.Internal.Search

import Types

-- have to implement this
matchPattern :: T.Text -> T.Text -> [Match]
matchPattern pattern text = 
                        let startPositions = indices pattern text
                            matchLength = T.length pattern
                        in [(pos, matchLength) | pos <- startPositions]