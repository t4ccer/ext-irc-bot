module Utils where

import           Data.List (isPrefixOf)

-- | Flipped veriosn of `isPrefixOf`. Useful to use as infix operator
startsWith :: Eq a => [a] -> [a] -> Bool
startsWith = flip isPrefixOf

-- | Extracts nickname from whole IRC username 
humanizeName :: String -> String
humanizeName =  head . wordsWhen ('!' ==) . head . wordsWhen ('@' ==)

-- | Splits `String` by predicate
wordsWhen :: (Char -> Bool) -> String -> [String]
wordsWhen p s =  case dropWhile p s of
    "" -> []
    s' -> w : wordsWhen p s''
        where (w, s'') = break p s'
