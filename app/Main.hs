module Main where

import qualified System.Environment as Env
import qualified Data.Text as T
import System.Directory

import Search
import Types

getFlags :: [String] -> [Flags]
getFlags = map (\f -> case f of
                        "-r" -> Recursive
                        "-n" -> LineNumber
                        "-f" -> ShowFile
                        "-i" -> CaseInSensitive
                        _    -> Recursive
                        )

parseArgs :: [String] -> [String] -> IO (Either ErrorMsg Args)
parseArgs flags nonflags = case nonflags of 
                                []          -> pure $ Left ("No pattern provided")
                                [pattern]   -> pure $ Right (Args{
                                                    pattern= T.pack pattern,
                                                    searchLocation= Nothing,
                                                    searchLocationType= STDIN,
                                                    flags= getFlags flags
                                                })
                                (pattern:location:_) -> do
                                                        isDirectory <- doesDirectoryExist location
                                                        isFile <- doesFileExist location

                                                        if not (isDirectory || isFile)
                                                            then pure $ Left ("Invalid search location: " ++ location)
                                                            else pure $ Right (Args{
                                                                pattern= T.pack pattern,
                                                                searchLocation= Just (T.pack location),
                                                                searchLocationType= FS,
                                                                flags= getFlags flags
                                                            })

main :: IO ()
main = do
    args <- Env.getArgs

    if null args
        then error("No arguments provided") else pure ()

    let
        flags = [arg | arg <- args, head arg == '-' ]
        nonflags = [arg | arg <- args, head arg /= '-' ]
    
    res <- parseArgs flags nonflags
    case res of
        Left errMsg -> error(errMsg)
        Right args  -> search args

