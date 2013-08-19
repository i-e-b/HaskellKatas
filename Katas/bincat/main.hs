import Data.Char
import Numeric

main = do
	file <- getContents
	putStrLn (bytesOf file)

bytesOf :: String -> String
bytesOf bs = concatMap (byteHex) (bs)
	where
		byteHex i = showIntAtBase 16 intToDigit (ord i) ""

