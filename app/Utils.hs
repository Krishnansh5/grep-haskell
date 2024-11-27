{-# OPTIONS_GHC -Wno-name-shadowing #-}
module Utils where

import qualified Data.Text.IO as T
import qualified Data.Text as T
import Text.Colour
import Data.Char (isSpace)
import Data.Maybe (fromMaybe)

import Types

parseInt :: String -> Int
parseInt x = read x :: Int

printMatches :: [MatchResult] -> IO()
printMatches [] = pure ()
printMatches (matchRes:rest) = do
                                let location' = renderChunkText With8Colours $ fore brightMagenta $ chunk $ fromMaybe (T.pack "") (location matchRes)
                                let ln = renderChunkText With8Colours $ fore brightGreen $ chunk $ T.pack $ maybe "" show (lineNumber matchRes)

                                let highlightedLine = applyHighlight (line matchRes) 0 (matches matchRes)
                                let lastMatch = last (matches matchRes)
                                let suffixStart = uncurry (+) lastMatch
                                let suffix = T.drop suffixStart (line matchRes)
                                let suffixHighlighted = renderChunkText With8Colours $ fore brightWhite $ chunk suffix

                                let contextBeforeColored = map (renderChunkText With8Colours . fore brightWhite . chunk) (contextBefore matchRes)
                                let contextBeforeText = map (\l -> location' <> T.pack "\t:: " <> T.pack "\t" <> l) contextBeforeColored

                                let contextAfterColored = map (renderChunkText With8Colours . fore brightWhite . chunk) (contextAfter matchRes)
                                let contextAfterText = map (\l -> location' <> T.pack "\t:: " <> T.pack "\t" <> l) contextAfterColored

                                mapM_ T.putStrLn contextBeforeText
                                T.putStrLn $ location' <> T.pack "\t:: " <> ln <> T.pack "\t" <> highlightedLine <> suffixHighlighted
                                mapM_ T.putStrLn contextAfterText

                                putStrLn ""

                                printMatches rest

                                where
                                    applyHighlight :: T.Text -> Int -> [Match] -> T.Text
                                    applyHighlight _ _ [] = T.pack ""
                                    applyHighlight line pos ((start, len):rest) =
                                        let
                                            prefix = T.take (start - pos) (T.drop pos line)
                                            highlight = T.take len (T.drop start line)
                                            prefix' = if isAllWhitespace prefix
                                                        then prefix
                                                        else renderChunkText With8Colours $ fore brightWhite $ chunk prefix
                                            highlight' = renderChunkText With8Colours $ fore brightYellow $ chunk highlight
                                            nextPos = start + len
                                            in prefix' <> highlight' <> applyHighlight line nextPos rest

                                    isAllWhitespace :: T.Text -> Bool
                                    isAllWhitespace = T.all isSpace

getContextAfterIfPresent :: [Flags] -> [T.Text] -> Int -> [T.Text]
getContextAfterIfPresent flags textLines ln = if ln == (length textLines-1)
                                                then []
                                                else case [n | ContextAfter n <- flags] of
                                                        (n:_) ->
                                                                let start = min (ln+1) (length textLines-1)
                                                                    end = min (ln+n) (length textLines-1)
                                                                in  [line | i <- [start..end], let line = textLines !! i]
                                                        []    -> []

getContextBeforeIfPresent :: [Flags] -> [T.Text] -> Int -> [T.Text]
getContextBeforeIfPresent flags textLines ln = if ln == 0
                                                then []
                                                else case [n | ContextBefore n <- flags] of
                                                        (n:_) ->
                                                                let end = max (ln-1) 0
                                                                    start = max (ln-n) 0
                                                                in  [line | i <- [start..end], let line = textLines !! i]
                                                        []    -> []