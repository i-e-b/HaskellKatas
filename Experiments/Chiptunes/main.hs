-- Chip tunes!
import Data.Bits
import Data.Word
import Data.ByteString.Lazy (ByteString, pack)
import qualified Data.ByteString.Lazy as B

main = return choons >>= B.putStr

choons :: ByteString
choons = repack $ map (\t -> t * (((t `shiftR` 12) .|. (t `shiftR` 8)) .&. (63 .&. (t `shiftR` 4)))) seed

repack :: [Word32] -> ByteString
repack x = pack $ map fromIntegral x

seed :: [Word32]
seed = [0..]
