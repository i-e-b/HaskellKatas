-- A simple, quick bloom filter, using murmur2 hash.
-- should require: cabal install bitset; but this isn't installing on windows.
-- will do a 64 bit forced version for now.
import Murmur
import BitSet
import Data.Bits
import Data.Word

type HashSeed = Word32
data Bloom = Bloom BitSet [HashSeed]


