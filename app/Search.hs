{-# OPTIONS_GHC -Wno-name-shadowing #-}
module Search where

import qualified Data.Text.IO as T
import qualified Data.Text as T
import Data.Maybe (fromMaybe)
import System.Directory
    ( doesDirectoryExist, doesFileExist, listDirectory )

import Types
import Match
import Utils

searchInText :: T.Text -> [T.Text] -> Maybe T.Text -> [Flags] -> IO()
searchInText pattern textLines searchLocation flags = do
                                            let (textLines',pattern') = if CaseInSensitive `elem` flags
                                                            then (map T.toLower textLines, T.toLower pattern)
                                                            else (textLines,pattern)
                                            let allMatches = map (findAllMatchesRegex pattern') textLines'

                                            let matchResults = [ MatchResult{
                                                                location= if ShowFile `elem` flags then searchLocation else Nothing,
                                                                lineNumber= if LineNumber `elem` flags then Just i else Nothing,
                                                                line= line,
                                                                matches= matches,
                                                                contextAfter= getContextAfterIfPresent flags textLines' i,contextBefore= getContextBeforeIfPresent flags textLines' i
                                                            }   |   i <- [0..(length allMatches-1)],
                                                                    let matches = allMatches !! i,
                                                                    let line = textLines' !! i,
                                                                    not (null matches) ]
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
                case searchLocationType args of
                    STDIN -> handleSTDIN args
                    FS -> handleFS args
