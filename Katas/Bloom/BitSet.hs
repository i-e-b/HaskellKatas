-- A very simplistic BitSet implementation.
-- Don't rely on it for anything serious
module BitSet (BitSet, empty, addBit, isSet, anySet, allSet, setOf, toRaw, setOR, setAND) where 

import Data.Bits
import Data.Word
import Data.List

newtype BitSet = BitSet [Word64] deriving (Show, Eq)

-- return a list of 64bit words used to store the bit set
toRaw :: BitSet -> [Word64]
toRaw (BitSet bs) = bs

-- empty bitstring, no elements set
empty :: BitSet
empty = BitSet []

-- given a bit position (x), return a BitSet with only that bit set
oneBit :: (Integral a) => a -> BitSet
oneBit x = BitSet (unfoldr (blocksFor x) 1)
	where
		blocksFor :: (Integral b) => b -> b -> Maybe (Word64, b)
		blocksFor index block 
			| block < 1 + (index `div` 64)  = Just (0, (block + 1))
			| block == 1 + (index `div` 64) = Just (fromIntegral 1 `shiftL` (fromIntegral index `mod` 64), (block + 1))
			| otherwise = Nothing

-- return a BitSet with all the listed bits set, others not set.
setOf :: (Integral a) => [a] -> BitSet
setOf list = make list empty
	where
		make :: (Integral a) => [a] -> BitSet -> BitSet
		make (x:xs) bs = make xs (addBit x bs)
		make [] bs = bs

-- return the input bitstring with bit (x) set, regardless of if it was set before.
addBit :: Integral a => a -> BitSet -> BitSet
addBit x bs = setOp (.|.) (oneBit x) bs

-- bitwise operation on two sets
setOp :: (Word64 -> Word64 -> Word64) -> BitSet -> BitSet -> BitSet
setOp f (BitSet a) (BitSet b) = BitSet (zipLong 0 (f) a b)

-- Join two sets with a boolean OR
setOR :: BitSet -> BitSet -> BitSet
setOR = setOp (.|.)

-- Join two sets with a boolean AND
setAND :: BitSet -> BitSet -> BitSet
setAND = setOp (.&.)

-- returns true if bit (x) of the bitstring is set
isSet :: Int -> BitSet -> Bool
isSet x (BitSet bs) = if exists then (bs !! block) .&. mask /= 0 else False
	where
		mask = 1 `shiftL` (x `mod` 64) :: Word64
		block = x `div` 64
		exists = (length bs) > block

-- given a set of positions, returns true if any are set
anySet :: Integral a => [a] -> BitSet -> Bool
anySet (x:xs) bs = if (isSet (fromIntegral x) bs) then True else anySet xs bs
anySet [] bs = False

-- given a set of positions, returns true iff all are set.
allSet :: Integral a => [a] -> BitSet -> Bool
allSet (x:xs) bs = if (not $ isSet (fromIntegral x) bs) then False else allSet xs bs
allSet [] bs = True

-- like zipWith, but uses length of longest list, not shortest
-- if lists are not equal, the id (x) is used as a proxy
zipLong :: a -> (a -> a -> c) -> [a] -> [a] -> [c]
zipLong p f (x:xs) (y:ys) = f x y : zipLong p f xs ys
zipLong p f []     (y:ys) = f p y : zipLong p f [] ys
zipLong p f (x:xs) []     = f x p : zipLong p f xs []
zipLong _ _ _ _ = []
