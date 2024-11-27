{-# OPTIONS_GHC -Wno-name-shadowing #-}
module Main where

import qualified System.Environment as Env
import qualified Data.Text as T
import System.Directory ( doesDirectoryExist, doesFileExist )
import Control.Monad (when)

import Search
import Types
import Utils
import Constants

parseArgs :: [Flags] -> [String] -> IO (Either ErrorMsg Args)
parseArgs flags' nonflags = case nonflags of
                                []          -> pure $ Left "No pattern provided"
                                [pattern']   -> pure $ Right (Args{
                                                    pattern= T.pack pattern',
                                                    searchLocation= Nothing,
                                                    searchLocationType= STDIN,
                                                    flags= flags'
                                                })
                                (pattern':location':_) -> do
                                                        isDirectory <- doesDirectoryExist location'
                                                        isFile <- doesFileExist location'

                                                        if not (isDirectory || isFile)
                                                            then pure $ Left ("Invalid search location: " ++ location')
                                                            else pure $ Right (Args{
                                                                pattern= T.pack pattern',
                                                                searchLocation= Just (T.pack location'),
                                                                searchLocationType= FS,
                                                                flags= flags'
                                                            })

separateFlagsAndNonFlags :: [String] -> ([Flags],[String])
separateFlagsAndNonFlags [] = ([],[])
separateFlagsAndNonFlags (arg:args) = if head arg == '-'
                                            then if flagWithValues arg
                                                    then
                                                        let (flags',nonflags') = separateFlagsAndNonFlags (tail args)
                                                            v = parseInt (head args)
                                                        in ( flagMap (arg,v):flags' , nonflags' )
                                                    else
                                                        let (flags',nonflags') = separateFlagsAndNonFlags args
                                                        in ( flagMap (arg,0):flags' , nonflags' )
                                            else (flags', arg:nonflags)
                                                 where (flags',nonflags) = separateFlagsAndNonFlags args
main :: IO ()
main = do
    args <- Env.getArgs

    when (null args) $ error "No arguments provided"

    let (flags,nonflags) = separateFlagsAndNonFlags args

    res <- parseArgs flags nonflags
    case res of
        Left errMsg -> error errMsg
        Right args  -> search args