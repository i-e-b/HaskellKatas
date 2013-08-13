-- Chip tunes!
import Data.Bits
import Data.Word
import Data.ByteString.Lazy (ByteString, pack)
import qualified Data.ByteString.Lazy as B

main = return choons >>= B.putStr

mapPack :: (Word32 -> Word32) -> ByteString
mapPack f = repack $ map f seed

soundFunc :: Word32 -> Word32
-- square wave with variable duty
soundFunc t = if ((t `mod` 100) < 20) then 0 else 255

-- output a sin-like waveform that takes a given number of samples to cycle
sineLikeWave :: Int -> [Int]
sineLikeWave rate =
	let step = (-3.14159265) + (6.28318531 / (fromIntegral rate))
	    angles = cycle [-3.14159265,step..3.14159265] -- the step size would be the rate
	in  map (\x -> floor $ 127.0 * (f x)) angles
	where f x = if (x < 0) then (1.27323954 * x + 0.405284735 * x * x) else (1.27323954 * x - 0.405284735 * x * x)

-- simple little tune
--soundFunc = (\t -> t * (((t `shiftR` 12) .|. (t `shiftR` 8)) .&. (63 .&. (t `shiftR` 4))))

bitwiseNoise :: [Word32]
bitwiseNoise = map mf (noise 2 256)
	where
		mf n = if (n .&. 1 == 1) then 255 else 0
		
-- roughly the NES noise func. A seed of 2 and len of 32768 is the default
-- a really short len give square wave tones. Seed has minor effects,
-- play with the len a lot
noise :: Word16 -> Int -> [Word16]
noise seed len = (concat . repeat . take len) $ noiseFunc seed
	where
		noiseFunc n = n : (noiseFunc $ nextN n)
		nextN n' = (n' `shiftR` 1) + ((n' .&. 1) `xor` ((n' .&. 2) `shiftR` 1) `shiftL` 14)


choons :: ByteString
--choons = repack bitwiseNoise
--choons = mapPack soundFunc
choons = pack127 $ sineLikeWave 18

--choons :: ByteString
--choons = repack $ map (\t -> t * (((t `shiftR` 12) .|. (t `shiftR` 8)) .&. (63 .&. (t `shiftR` 4)))) seed

pack127 :: [Int] -> ByteString
pack127 x = pack $ map (\i -> fromIntegral (i+127))  x

repack :: [Word32] -> ByteString
repack x = pack $ map fromIntegral x

seed :: [Word32]
seed = [0..]
