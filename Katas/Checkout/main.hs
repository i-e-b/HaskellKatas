{- Very basic version of the checkout kata

Item     Price     Offer
-------------------------------
 A       50        3 for 130
 B       30        2 for 45
 C       20
 D       15
 
Accept items in any order, apply matching offers
-}

import Data.List
import Test.HUnit

-- Unit tests

tests = TestList [ "empty"		~: (0)				~=? (priceOf "")
                 , "A"			~: (50)				~=? (priceOf "A")
                 , "AB"			~: (80)				~=? (priceOf "AB")
                 , "CDBA"		~: (115)			~=? (priceOf "CDBA")
                 , "AA"			~: (100)			~=? (priceOf "AA")
                 , "AAA"		~: (130)			~=? (priceOf "AAA")
                 , "AAABB"		~: (175)			~=? (priceOf "AAABB")
                 , "ACAABCB"	~: (215)			~=? (priceOf "ACAABCB")
				 , "AAA A"		~: (180)			~=? (priceOf "AAAA")
				 , "BB B"		~: (75)				~=? (priceOf "BBB")
				 , "AAA AAA A"	~: (310)			~=? (priceOf "AAAAAAA")
                 ]
--
-- On execute, run tests.
main = do
	runTestTT tests
	
-- Implementation

-- priceOf: determines the total cost of a basket of codes.
-- sorts the codes, applies pricing, sums result
priceOf :: String -> Int
priceOf str = sum $ prices (Just $ sort str) priceList

-- List of all prices, longest deals first
priceList :: [(String, Int)]
priceList = [("AAA", 130), ("BB", 45), ("A", 50), ("B", 30), ("C", 20), ("D", 15)]

-- Finds all the prices in a basket
-- Given a string and tuples of substrings and ints, list all matched ints
prices :: Maybe String -> [(String, Int)] -> [Int]
prices Nothing _ = undefined
prices _ [] = []
prices (Just []) _ = []
prices (Just str) pl = 
	case (prefixLookup str pl) of
		Nothing -> []
		Just (prefix,val) -> val : (prices (stripPrefix prefix str) pl)

-- Given a list and tuples of (prefix, value) return first matched tuple or nothing.
-- Treated here as String -> [(String, Int)] -> Maybe (String, Int)
prefixLookup :: (Eq a) => [a] -> [([a], b)] -> Maybe ([a],b)
prefixLookup [] _ = Nothing
prefixLookup _ [] = Nothing
prefixLookup list (found@(prefix, value):rest) = if (prefix `isPrefixOf` list) then (Just found) else prefixLookup list rest
