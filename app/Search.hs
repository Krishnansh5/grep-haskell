module Search where

import qualified Data.Text.IO as T
import qualified Data.Text as T
import Text.Colour
import Data.Maybe (fromMaybe)
import Data.List (foldl')
import Data.Char (isSpace)
import System.Directory

import Debug.Trace (trace)

import Types
import Regex

printMatches :: [MatchResult] -> IO()
printMatches [] = pure ()
printMatches (matchRes:rest) = do
                                let location' = renderChunkText With8Colours $ fore brightMagenta $ chunk $ (fromMaybe (T.pack "") (location matchRes))
                                let ln = renderChunkText With8Colours $ fore brightGreen $ chunk $ T.pack $ (maybe "" show (lineNumber matchRes))

                                let highlightedLine = foldl' (applyHighlight (line matchRes)) T.empty (matches matchRes)
                                let lastMatch = last(matches matchRes)
                                let suffixStart = (fst lastMatch) + (snd lastMatch)
                                let suffix = T.drop suffixStart (line matchRes)
                                let suffixHighlighted = renderChunkText With8Colours $ fore brightWhite $ chunk suffix

                                T.putStrLn $ location' <> (T.pack "\t:: ") <> ln <> (T.pack "\t") <> highlightedLine <> suffixHighlighted

                                printMatches rest

                                where
                                    applyHighlight :: T.Text -> T.Text -> Match -> T.Text
                                    applyHighlight line processed (start, len) =
                                        let processedLength = T.length processed
                                            prefix = T.take (start - processedLength) (T.drop processedLength line)
                                            highlight = T.take len (T.drop start line)
                                            prefix' = if (isAllWhitespace prefix)
                                                        then renderChunkText With8Colours $ fore brightWhite $ chunk prefix
                                                        else prefix
                                            highlight' = renderChunkText With8Colours $ fore brightYellow $ chunk highlight
                                        in processed <> prefix' <> highlight'
                                    
                                    isAllWhitespace :: T.Text -> Bool
                                    isAllWhitespace = T.all isSpace

searchInText :: T.Text -> [T.Text] -> (Maybe T.Text) -> [Flags] -> IO()
searchInText pattern textLines searchLocation flags = do
                                            let (textLines',pattern') = if CaseInSensitive `elem` flags
                                                            then (map T.toLower textLines, T.toLower pattern)
                                                            else (textLines,pattern)
                                            let allMatches = map (matchPattern pattern') textLines'

                                            let matchResults = [ MatchResult{
                                                                location= if (ShowFile `elem` flags) then searchLocation else Nothing,
                                                                lineNumber= if (LineNumber `elem` flags) then Just i else Nothing,
                                                                line= line,
                                                                matches= matches
                                                            }   |   i <- [0..(length(allMatches)-1)],
                                                                    let matches = (allMatches !! i),
                                                                    let line = (textLines' !! i),
                                                                    length(matches) > 0 ]
                                            -- print (show matchResults)
                                            printMatches matchResults

handleSTDIN :: Args -> IO()
handleSTDIN args = do
                        text <- T.getContents
                        let lines = T.lines text

                        searchInText (pattern args) lines Nothing (flags args)

handleFS :: Args -> IO()
handleFS args = do
                    let location = T.unpack (fromMaybe (T.pack "") (searchLocation args))
                    isDirectory <- doesDirectoryExist location
                    if isDirectory
                        then
                            processDirectory location
                        else processFile location
                    where
                        processContent :: String -> IO() 
                        processContent content = do 
                                                    isFile <- doesFileExist content
                                                    if isFile
                                                        then processFile content
                                                        else processDirectory content
                                                                
                        processFile :: String -> IO()
                        processFile fileName = do
                                                text <- T.readFile fileName
                                                let lines = T.lines text
                                                searchInText (pattern args) lines (Just (T.pack fileName)) (flags args)

                        processDirectory :: String -> IO()
                        processDirectory dir = do
                                                contents_ <- listDirectory dir
                                                let contents = map (\c -> dir ++ "/" ++ c) contents_
                                                mapM_ processContent contents

search :: Args -> IO()
search args = do
                case (searchLocationType args) of
                    STDIN -> handleSTDIN args
                    FS -> handleFS args
                    