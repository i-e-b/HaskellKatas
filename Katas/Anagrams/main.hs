-- Anagrams
import Data.List
import Data.Maybe
import Data.Function
import Data.Traversable
import Control.Applicative
import qualified Data.Map as M

type Dictionary = [String]
type GroupedDictionary = [(Int, Dictionary)] -- (length of each entry, entries)
type WordList = [String]


data Match = Match { remains :: [Char]             -- the remaining characters of the input. At root, this is the input. At leafs it is []
                   , matches :: M.Map String Match -- map of dictionary word matched to sub-matches
				   } deriving (Show)
--

main :: IO ()
main = do
	putStrLn "Source sentence:"
	content <- readFile "TinyDictionary.txt"
	input <- getLine
	putStrLn . unlines $ anagrams (groupedDictionary . words $ content) input
	
anagrams :: GroupedDictionary -> String -> WordList
anagrams dic inpStr = concat $ unroll (buildMatchTree dic (crush inpStr))

-- traverse a match tree and build lists of dictionary word paths
-- any paths that end in non-empty 'remains' are invalid.
unroll :: Match -> [WordList]
unroll = undefined 
-- Depth-first-search. Where on a leaf, if (deadEnd) then (nothing) else (add to accumulator)

-- something like this?
accumResults :: [WordList] -> Match -> [WordList]
accumResults acc m = [ accumResults (acc ++ (peel x)) ...? | x <- matches m, not deadend m ...]

isLeaf :: Match -> Bool
isLeaf m = M.null (matches m)

-- If true, this is the leaf of a path that is no good
deadEnd :: Match -> Bool
deadEnd m = (remains m /= []) && (M.keys (matches m) == []) -- end of the match path, but letters left over.

-- get all the dictionary words at a single match point
getPointWords :: Match -> [String]
getPointWords mtc = M.keys $ matches mtc

-- build the match tree. at each level, finds all single dictionary words contained in the 
--  remaining string, and starts a new match sub tree with the matched word removed from 
--  the remaining string.
buildMatchTree :: GroupedDictionary -> String -> Match
buildMatchTree gDic srcStr = m gDic srcStr
	where
		m :: GroupedDictionary -> String -> Match
		m dic s = Match {remains = s, matches = (mf dic s)}

		mf :: GroupedDictionary -> String -> M.Map String Match
		mf _ [] = M.empty
		mf dic remains = M.fromList [ (x, m dic (remains \\ x)) | x <- (withinSize dic remains), remains `containsWord` x ]
		
		withinSize d str = concatMap (snd) $ takeWhile (lte str) d
		lte s = (<= (length s)) . fst
		containsWord remains = all (`elem` remains)  -- "hello world" `containsWord` "ol dw" == true

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
