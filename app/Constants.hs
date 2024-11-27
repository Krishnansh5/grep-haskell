module Constants where

import Types

flagWithValues :: String -> Bool
flagWithValues f = f `elem` ["-A","-B"]

flagMap :: (String,Int) -> Flags
flagMap (f,v) = case f of
                    "-r" -> Recursive
                    "-n" -> LineNumber
                    "-l" -> ShowFile
                    "-i" -> CaseInSensitive
                    "-A" -> ContextAfter v
                    "-B" -> ContextBefore v
                    _    -> Recursive