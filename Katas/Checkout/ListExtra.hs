module ListExtra (prefixLookup) where 
import Data.List

-- Given a list and tuples of (prefix, value) return first matched tuple or nothing.
prefixLookup :: (Eq a) => [a] -> [([a], b)] -> Maybe ([a],b)
prefixLookup [] _ = Nothing
prefixLookup _ [] = Nothing
prefixLookup list (found@(prefix, value):rest) = if (prefix `isPrefixOf` list) then (Just found) else prefixLookup list rest