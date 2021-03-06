-- Anagrams
import Data.List
import Data.Maybe
import Data.Function
import Control.Monad.Writer
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
anagrams dic inpStr = nub . readResults $ (buildMatchTree dic $ crush inpStr)
--

-- Read the successful match paths into a list of strings
readResults :: Match -> [String]
readResults m = snd $ runWriter (accumResults [] m)

-- Accumulate successful paths into a Writer.
-- Works by following all paths through the tree accumulating the paths along 
--  the way, and writing with 'tell' when a successful full path is found ('isLeaf')
-- Non-successful paths (where there were letters left over with no dictionary matches)
--  are ignored with no write.
accumResults :: [String] -> Match -> Writer [String] ()
accumResults path match
	| isLeaf match = tell [(unwords . sort $ path)]
	| deadEnd match = return ()
	| otherwise = mapM_ (\(p, m) -> accumResults (p:path) m) (M.toList . matches $ match)
	where
		isLeaf :: Match -> Bool -- found a word set for all letters (no letters left, and is a leaf)
		isLeaf m = (M.null (matches m)) && (remains m == [])
		
		deadEnd :: Match -> Bool -- If true, this is the leaf of a path that is no good
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
		mf dic remains = M.fromList [ (x, m dic (remains \\ x)) | x <- (withinSize dic remains), isIntersect x remains]
		
		withinSize d str = concatMap (snd) $ takeWhile (lte str) d
		lte s = (<= (length s)) . fst

-- true iff haystack contains the elements of needle in gte number
isIntersect :: (Eq a) => [a] -> [a] -> Bool
isIntersect n [] = n == []
isIntersect [] _ = True
isIntersect (needle:ns) haystack = if (needle `elem` haystack) then (isIntersect ns (delete needle haystack)) else False

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
