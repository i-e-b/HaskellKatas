-- A very simplistic BitSet implementation.
-- Don't rely on it for anything serious
module BitSet (empty, set, isSet, anySet, allSet,    oneBit) where 

import Data.Bits
import Data.Word
import Data.List

newtype BitSet = BitSet [Word64] deriving (Show, Eq)

empty :: BitSet
empty = BitSet []

oneBit :: (Integral a) => a -> BitSet
oneBit x = BitSet (unfoldr (blocksFor x) 1)


blocksFor :: (Integral b) => b -> b -> Maybe (Word64, b)
blocksFor index block 
	| block < 1 + (index `div` 64)  = Just (0, (block + 1))
	| block == 1 + (index `div` 64) = Just (fromIntegral 1 `shiftL` (fromIntegral index `mod` 64), (block + 1))
	| otherwise = Nothing

set :: Ord a => a  -> BitSet -> BitSet
set x bs = bs

isSet :: Ord a => a  -> BitSet -> Bool
isSet x bs = False

anySet :: Ord a => [a] -> BitSet -> Bool
anySet xs bs = False

allSet :: Ord a => [a] -> BitSet -> Bool
allSet xs bs = False
