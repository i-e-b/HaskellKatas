{- 
	Bellringing kata in Haskell.
	
	See Readme for details.
-}

import Test.HUnit
import Data.List

-- Unit tests

bellsAtPosition x = bells !! x
tests = TestList [ "initital position 1"	~: ([1])				~=? (take 1 $ bellsAtPosition 0)
                 , "initital position 2"	~: ([2])				~=? (take 1 $ bellsAtPosition 1)
				 , "initial round"			~: ([1,2,3,4,5,6,7,8])	~=? (bellsAtPosition 0)
				 , "round 1"				~: ([2,1,4,3,6,5,8,7])	~=? (bellsAtPosition 1)
				 , "round 2"				~: ([2,4,1,6,3,8,5,7])	~=? (bellsAtPosition 2)
				 , "round 15"				~: ([1,3,2,5,4,7,6,8])	~=? (bellsAtPosition 15)
				 , "round 16"				~: ([1,3,5,2,7,4,8,6])	~=? (bellsAtPosition 16)
				 , "round 112"				~: ([1,2,3,4,5,6,7,8])	~=? (bellsAtPosition 112)
                 ]

main = do
	runTestTT tests

-- Settings
start = [1..8]										-- The initial list of bell pullers
pattern = (7 `_of` [[], [1,8]]  ) ++ [[], [1,2]]	-- 'hold' patterns. 1-based indexes of pullers to stay put

-- Implementation

pullPattern :: [[Int]]
pullPattern = cycle pattern

bells = [start] ++ (bellLoop pullPattern start)

-- permuteByPattern takes a pull pattern and a current arrangement, returns next arrangement.
permuteByPattern :: [Int] -> [a] -> [a]
permuteByPattern pat inp = (map unscrew) $ swapPairs (decide pat inp)
	where
		decide pat inp = [ if (fst x `elem` pat) then Left (snd x) else Right (snd x) | x <- zip [1..] inp ] -- left if held, right if movable
		unscrew = either id id -- lifts out of Either

-- Swap pairs of movable pullers. This won't swap two movable pullers either side of a held puller
swapPairs :: [Either a b] -> [Either a b]
swapPairs ((Right x):(Right y):xs) = (Right y):(Right x):(swapPairs xs)
swapPairs (x:xs) = x:(swapPairs xs)
swapPairs [] = []

-- Spew an endless list of the permutations
bellLoop :: [[Int]] -> [a] -> [[a]]
bellLoop (pat:xs) current = 
	let next = permuteByPattern pat current
	in  [next] ++ (bellLoop xs next)

-- Little helpers
_of x = concat . replicate x

