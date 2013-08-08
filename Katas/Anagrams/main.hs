-- Anagrams with hashes
-- 
-- The basic idea is to have a dictionary lookup of (word sorted)->(word)
-- then to sort the input and take varying length slices looking for entries in the dict map.

import Data.List
import Data.Maybe
import Data.Function
import qualified Data.Map as M

type Dictionary = [String]
type GroupedDictionary = [(Int, Dictionary)] -- (length of each entry, entries)
type WordList = [String]

data Match = Match { remains :: [Char]             -- the remaining characters of the input. At root, this is the input. At leafs it is []
                   , matches :: M.Map String Match -- map of dictionary word matched to sub-matches
				   }

main :: IO ()
main = do
	putStrLn "Source sentence:"
	content <- readFile "TinyDictionary.txt"
	input <- getLine
	putStrLn . unlines $ anagrams (groupedDictionary . words $ content) input
	
anagrams :: GroupedDictionary -> String -> WordList
anagrams gd s = map show gd

m :: GroupedDictionary -> String -> Match
m gd s = Match {remains = s, matches = (mf gd s)}

-- for all dicts less than string length, Map (dictWord -> m gd s-with-dict-Word-removed)
mf :: GroupedDictionary -> String -> M.Map String Match
mf gd s = 
-- fromList [("word", m gd (s \\ "word")), ("glob", m gd (s \\ "glob")]
-- "hello" \\ "oel" == "hl"
-- containsWord remains = and . map (`elem` remains)  -- > "hello world" `containsWord` "world"


-- Take a list of words and give a list where words of the same length are grouped together, with that length
groupedDictionary :: WordList -> GroupedDictionary
groupedDictionary wl =
	let g = dictByLength wl
	in  zip (lengths g) g

-- Lengths of words in each group
lengths :: [Dictionary] -> [Int]
lengths = map (length . head)

-- Group a list of strings by lengths. (really, type is [[a]] -> [[[a]]])
dictByLength :: Dictionary -> [Dictionary]
dictByLength = groupBy ((==) `on` length) . sortBy (compare `on` length)
