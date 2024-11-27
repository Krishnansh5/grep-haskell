module Types where

import qualified Data.Text as T

type ErrorMsg = String
type Match = (Int,Int) -- match start index , match length

data LocationType = FS | STDIN deriving (Eq,Show)
data Flags        = Recursive | LineNumber | ShowFile | CaseInSensitive | ContextAfter Int | ContextBefore Int deriving (Eq,Show)

data Args = Args {
    pattern             ::  T.Text,
    searchLocation      ::  Maybe T.Text,
    searchLocationType  ::  LocationType,
    flags               ::  [Flags]
} deriving (Eq, Show)

data MatchResult = MatchResult {
    location        ::  Maybe T.Text,
    lineNumber      ::  Maybe Int,
    line            ::  T.Text,
    matches         ::  [Match],
    contextAfter    ::  [T.Text],
    contextBefore   ::  [T.Text]
} deriving (Eq, Show)