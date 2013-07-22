-- A simple, quick bloom filter, using murmur2 hash.
-- should require: cabal install bitset; but this isn't installing on windows.
-- will do a 64 bit forced version for now.
import Murmur
import BitSet
import Data.Bits
import Data.Word
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as Convert

type HashSeed = Word32
data Bloom = Bloom
	{ set :: BitSet
	, seeds ::[HashSeed]
	, size :: Int } deriving (Show, Eq)

-- return a default empty bloom filter of size 1000 with 10 hashes
emptyBloom :: Bloom
emptyBloom = sizedBloom 1000 10

-- create an empty filter with the same hash seeds and size of another bloom filter
emptyClone :: Bloom -> Bloom
emptyClone b = Bloom {set = empty, seeds = seeds b, size = size b}
	
-- Given a count of hash functions and a maximum bitset size,
-- return an empty bloom filter. 1000 and 10 are reasonable defaults
sizedBloom :: Int -> Int -> Bloom
sizedBloom maxSize hashCount = Bloom {set = empty, seeds = take hashCount seededList, size = maxSize}

-- Add a byte-string value to a bloom filter
addString :: Bloom -> String -> Bloom
addString bl str = Bloom {set = ((set bl) `setOR` (setOf bitsToSet)), seeds = (seeds bl), size = (size bl)}
	where
		bitsToSet = map (limitForBloom bl) $ zipWith (murmur32) (repeat $ B.unpack (Convert.pack str)) (seeds bl)

-- Look for a value in a bloom filter
mightContain :: Bloom -> String -> Bool
mightContain inp str = (set inp) `setAND` (set $ addString (emptyClone inp) str) == (set inp)
		
-- Limit a hash value to the max size of a bloom filter
limitForBloom :: Bloom -> Word32 -> Int
limitForBloom bl a = (fromIntegral a) `mod` (size bl)