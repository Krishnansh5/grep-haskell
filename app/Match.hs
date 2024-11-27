{-# OPTIONS_GHC -Wno-name-shadowing #-}
module Match where

import qualified Data.Text as T
import Data.Text.Internal.Search ( indices )
import Text.Regex.TDFA ( (=~), AllMatches(getAllMatches) )

import Types

findAllMatches :: T.Text -> T.Text -> [Match]
findAllMatches pattern text =
                        let startPositions = indices pattern text
                            matchLength = T.length pattern
                        in [(pos, matchLength) | pos <- startPositions]

findAllMatchesRegex :: T.Text -> T.Text -> [Match]
findAllMatchesRegex pattern text = getAllMatches (text =~ pattern :: AllMatches [] Match)
