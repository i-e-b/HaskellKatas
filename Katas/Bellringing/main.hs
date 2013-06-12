{- 
	Bellringing kata in Haskell.
	
	See Readme for details.
-}

import Test.HUnit
import Data.List

tests = TestList [ "initital position 1"	~: ([1])				~=? (take 1 $ bellsAtPosition 0)
                 , "initital position 2"	~: ([2])				~=? (take 1 $ bellsAtPosition 1)
				 , "initial round"			~: ([1,2,3,4,5,6,7,8])	~=? (bellsAtPosition 0)
				 , "round 1"				~: ([2,1,4,3,6,5,8,7])	~=? (bellsAtPosition 1)
				 , "round 2"				~: ([2,4,1,6,3,8,5,7])	~=? (bellsAtPosition 2)
				 , "round 15"				~: ([1,3,2,5,4,7,6,8])	~=? (bellsAtPosition 15)
				 , "round 16"				~: ([1,3,5,2,7,4,8,6])	~=? (bellsAtPosition 16)
				 , "round 112"				~: ([1,2,3,4,5,6,7,8])	~=? (bellsAtPosition 112)
                 ]
--


-- bells, with a wildcard at 15.
-- TODO: generalise this.
bellsAtPosition :: Int -> [Integer]
bellsAtPosition x = bells !! x
bells = (take 16 $ bellSimple [1..8]) ++ (bellSimple (swap'' $ (bellSimple [1..8]) !! 15))
bellSimple = concat . unfoldr (\ ls -> Just ([ls, swap ls], swap'(swap ls))) -- bell swapping with no wildcard
swap (a:b:c:d:e:f:g:h:xs) = b:a:d:c:f:e:h:g:xs
swap' (a:b:c:d:e:f:g:h:xs) = a:c:b:e:d:g:f:h:xs
swap'' (a:b:c:d:e:f:g:h:xs) = a:b:d:c:f:e:h:g:xs

main = do
	runTestTT tests
