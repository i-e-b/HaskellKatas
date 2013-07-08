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
-- sorts and groups the codes, applies deals (which fall through to
--   item-by-item pricing)
priceOf :: String -> Int
priceOf str = sum $ map (deals) (group $ sort str)

-- deals: takes a string of items, checks for deals
-- falls through to itemised pricing if none found
deals :: String -> Int
deals ('A':'A':'A':more) = 130 + deals more -- is there a nicer way to pattern match this?
deals ('B':'B':more) = 45 + deals more
deals other = sum $ map price other

-- itemised pricing
price :: Char -> Int
price 'A' = 50
price 'B' = 30
price 'C' = 20
price 'D' = 15
price _ = undefined
