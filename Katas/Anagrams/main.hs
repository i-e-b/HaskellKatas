-- Very basic naiive attempt first.

import Data.List

main :: IO ()
main = do
	putStrLn "Source sentence:"
	content <- readFile "TinyDictionary.txt"
	input <- getLine
	putStrLn . unwords $ anagrams (words content) input

-- given a dictionary and an input, output all anagrams of the input
anagrams :: [String] -> String -> [String]
anagrams dict inp = [x | x <- (permutations $ crush inp), x `elem` dict]  -- very simple first pass, finds single words.

-- remove whitespace from a string
crush :: String -> String
crush = concat . words