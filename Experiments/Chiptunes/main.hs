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


-- simple little tune
--soundFunc = (\t -> t * (((t `shiftR` 12) .|. (t `shiftR` 8)) .&. (63 .&. (t `shiftR` 4))))





choons :: ByteString
choons = mapPack soundFunc

--choons :: ByteString
--choons = repack $ map (\t -> t * (((t `shiftR` 12) .|. (t `shiftR` 8)) .&. (63 .&. (t `shiftR` 4)))) seed

repack :: [Word32] -> ByteString
repack x = pack $ map fromIntegral x

seed :: [Word32]
seed = [0..]
