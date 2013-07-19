-- A simple, quick bloom filter, using murmur2 hash.

import Data.List
import Data.Word
import Data.Bits

-- Hash a list of bytes, given a seed (0x9747b28c is a reasonable seed)
murmur32 :: [Word8] -> Word32 -> Word32
murmur32 bytes seed = murmurRounds (split4 bytes) h
	where
		len = fromIntegral (length bytes) :: Word32
		h = seed `xor` len

-- Todo: generalise this
-- Split a list into 4 equal parts. Spares go into last item
split4 :: [a] -> ([a],[a],[a],[a])
split4 [] = ([],[],[],[])
split4 i = (f n 0 i, f n n i, f n (2 * n) i, f (2 * n) (3 * n) i)
	where
		n = (length i) `div` 4
		f a b = take a . drop b

mix = 0x5bd1e995

p' :: Word8 -> Word32
p' = fromIntegral

-- Perform all rounds on the data, given a starting hash
murmurRounds :: ([Word8],[Word8],[Word8],[Word8]) -> Word32 -> Word32
murmurRounds ((w:ws), (x:xs), (y:ys), (z:zs)) hash = murmurRounds (ws, xs, ys, zs) (murmurRound (w, x, y, z) hash)
murmurRounds ([],[],[], x:y:z:[]) hash = murmurRounds ([],[],[], x:y:[]) (hash `xor` (p' z `shiftL` 16))
murmurRounds ([],[],[], x:y:[]) hash = murmurRounds ([],[],[], x:[]) (hash `xor` (p' y `shiftL` 16))
murmurRounds ([],[],[], x:[]) hash = murmurRounds ([],[],[],[]) ((hash `xor` p' x) * mix)
murmurRounds _ hash = 
	let h' = (hash `xor` (hash `shiftR` 13)) * mix
	in  h' `xor` (h' `shiftR` 15) 
		
-- Single round of murmur, take tuple of 4 bytes of data (k)
--  and ongoing hash result (h)
murmurRound :: (Word8, Word8, Word8, Word8) -> Word32 -> Word32
murmurRound (w, x, y, z) hash = h'
	where
		join a b c d = (p' a) + ((p' b) `shiftL` 8) + ((p' c) `shiftL` 16) + ((p' d) `shiftL` 24) 
		k = (join w x y z)
		r = 24
		a = (k * mix) `xor` (k `rotateR` r)
		k' = a * mix
		h' = (hash * mix) `xor` k'
		
		