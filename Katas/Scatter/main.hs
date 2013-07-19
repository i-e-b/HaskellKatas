-- Scatter : some bit twiddling
-- given an int n < 0xFFFFFFFF, returns a list of 0..n-1 in a predictable but scattered order.
-- sort (scatter n) == [0..n-1]
import Data.Maybe
import Data.Word
import Data.Bits

--- TESTS ---
import qualified Test.QuickCheck as Q
import Data.List

wn :: Integer -> Word32
wn n = fromIntegral (n-1)

prop_SizeAndMembers n =  (n > 0) Q.==> (sort $ scatter n) == ([0..(wn n)])
prop_NotOrdered n =      (n > 3) Q.==> (scatter n)        /= ([0..(wn n)])
prop_NotReversed n =     (n > 3) Q.==> (scatter n)        /= ([(wn n)..0])
prop_NonNatural n =      (n < 1) Q.==> (scatter n)        == []

check_checkAll = do
	Q.quickCheck prop_SizeAndMembers
	Q.quickCheck prop_NotOrdered
	Q.quickCheck prop_NotReversed
	Q.quickCheck prop_NonNatural

-- Implementation

-- scatter for scale n
scatter :: Integer -> [Word32]
scatter n = take (fromIntegral n) [ x - 1 | x <- scatterSequence (magic n) 1, (toInteger x) <= n]

-- magic number, current position, all output; produces an infinite repeating list
scatterSequence :: Maybe Word32 -> Word32 -> [Word32]
scatterSequence Nothing _ = []
scatterSequence (Just m) value = -- todo: try re-writing this as an unfold.
	let value' =
		if ((value .&. 1) /= 0)
			then ((value `shiftR` 1) `xor` m)
			else (value `shiftR` 1)
	in  (value'):(scatterSequence (Just m) value')

-- pick a 'magic number' for scale n
magic :: Integer -> Maybe Word32
magic n 
	| n > 0 = listToMaybe [ magic | (scale, magic) <- mn, n < scale]
	| otherwise = Nothing
	where 
		mn :: [(Integer,Word32)]
		mn = [(0x00000004,0x00000003),(0x00000008,0x00000006),(0x00000010,0x0000000C),(0x00000020,0x00000014),
		      (0x00000040,0x00000030),(0x00000080,0x00000060),(0x00000100,0x000000B8),(0x00000200,0x00000110),
		      (0x00000400,0x00000240),(0x00000800,0x00000500),(0x00001000,0x00000CA0),(0x00002000,0x00001B00),
		      (0x00004000,0x00003500),(0x00008000,0x00006000),(0x00010000,0x0000B400),(0x00020000,0x00012000),
		      (0x00040000,0x00020400),(0x00080000,0x00072000),(0x00100000,0x00090000),(0x00200000,0x00140000),
		      (0x00400000,0x00300000),(0x00800000,0x00400000),(0x01000000,0x00D80000),(0x02000000,0x01200000),
		      (0x04000000,0x03880000),(0x08000000,0x07200000),(0x10000000,0x09000000),(0x20000000,0x14000000),
		      (0x40000000,0x32800000),(0x80000000,0x48000000),(0xFFFFFFFF,0xA3000000)]
--