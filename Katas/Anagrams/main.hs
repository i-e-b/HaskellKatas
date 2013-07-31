-- Very basic naiive attempt first.

import Data.List
import Data.Maybe

type Dictionary = [String]
type WordList = [String]

main :: IO ()
main = do
	putStrLn "Source sentence:"
	content <- readFile "TinyDictionary.txt"
	input <- getLine
	putStrLn . unwords $ anagrams (words content) input

-- given a dictionary and an input, output all anagrams of the input
anagrams :: Dictionary -> String -> WordList
anagrams dict inp = map unwords $ catMaybes [(findDictWords dict x) | x <- (permutations $ crush inp)] -- finds only the first matches, finds duplicates

findDictWords :: Dictionary -> String -> Maybe WordList
findDictWords = dictionaryWords (Just [])

-- given an accumulator, a dictionary, and an input string;
-- returns either a list of dictionary words that form the input string, or nothing
dictionaryWords :: Maybe WordList -> Dictionary -> String -> Maybe WordList
dictionaryWords Nothing _ _ = Nothing
dictionaryWords (Just accum) _ [] = Just accum
dictionaryWords (Just accum) dict inp = 
	let nextWord = ripWord dict inp
	    restOfString = drop (length nextWord) inp
	in  dictionaryWords (nextAccum nextWord) dict restOfString
	where
		nextAccum [] = Nothing
		nextAccum str = Just (str : accum)
-- 

-- Find a single dictionary word at the start of a string
ripWord :: Dictionary -> String -> String
ripWord dict str = 
	let word = take 1 $ inDict (inits str)
	in  if (length word > 0) then (word !! 0) else []
	where
		inDict strs = filter (`elem` dict) strs

-- remove whitespace from a string
crush :: String -> String
crush = concat . words