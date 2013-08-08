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
				   } deriving (Show)

main :: IO ()
main = do
	putStrLn "Source sentence:"
	content <- readFile "TinyDictionary.txt"
	input <- getLine
	putStrLn . unlines $ anagrams (groupedDictionary . words $ content) input
	
anagrams :: GroupedDictionary -> String -> WordList
anagrams dic inpStr = unroll (m dic (crush inpStr))

unroll :: Match -> WordList
unroll = undefined

-- (m) and (mf) mutually build the match tree
m :: GroupedDictionary -> String -> Match
m dic s = Match {remains = s, matches = (mf dic s)}

-- for all dicts less than string length, Map (dictWord -> m gd s-with-dict-Word-removed)
mf :: GroupedDictionary -> String -> M.Map String Match
mf _ [] = M.empty
mf dic remains = M.fromList [ (x, m dic (remains \\ x)) | x <- (withinSize dic remains), remains `containsWord` x ]
	where
		withinSize d str = concatMap (snd) $ takeWhile (lte str) d
		lte s = (<= (length s)) . fst
		containsWord remains = all (`elem` remains)  -- "hello world" `containsWord` "ol dw" == true

-- fromList [("word", m gd (s \\ "word")), ("glob", m gd (s \\ "glob")]
-- "hello" \\ "oel" == "hl"
--lkup dic remains = [ (x, remains \\ x) | x <- dic, remains `containsWord` x ]


-- remove whitespace from a string
crush :: String -> String
crush = concat . words

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
