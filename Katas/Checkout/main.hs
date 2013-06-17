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
                 ]
--
-- On execute, run tests.
main = do
	runTestTT tests
	
-- Implementation

priceOf :: String -> Int
priceOf str = sum $ map (deals) (group $ sort str)

deals :: String -> Int
deals "AAA" = 130
deals "BB" = 45
deals other = sum $ map price other
	where
		price 'A' = 50
		price 'B' = 30
		price 'C' = 20
		price 'D' = 15
		price _ = undefined
