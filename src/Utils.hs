module Utils where

import           Data.List (isPrefixOf)

startsWith :: Eq a => [a] -> [a] -> Bool
startsWith = flip isPrefixOf

humanizeName :: String -> String
humanizeName =  head . wordsWhen ('!' ==) . head . wordsWhen ('@' ==)

wordsWhen :: (Char -> Bool) -> String -> [String]
wordsWhen p s =  case dropWhile p s of
    "" -> []
    s' -> w : wordsWhen p s''
        where (w, s'') = break p s'
