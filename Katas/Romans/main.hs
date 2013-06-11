import qualified Data.List as T

main = do
	putStrLn "Give me a natural number"
	numinp <- getLine
	let n = read numinp :: Int
	putStrLn . romanise $ n

romanise :: Int -> String
romanise = optimise . numerals [(1000, 'M'), (500, 'D'),(100, 'C'),(50, 'L'),(10, 'X'),(5, 'V'),(1, 'I')] ""

numerals :: [(Int,Char)] -> String -> Int -> String
numerals ((scale, chr):xs) s i
	| i >= scale = numerals ((scale,chr):xs) (s ++ [chr]) (i - scale)
	| otherwise  = numerals xs s i
numerals [] s i = s

optimise :: String -> String
optimise inp = T.concat . optimiseGroups . T.group $ inp

optimiseGroups :: [String] -> [String]
optimiseGroups x = map subopt x 

subopt :: String -> String
subopt "IIII" = "IV"
subopt "III" = "IIV"
subopt "XXXX" = "XL"
subopt "CCCC" = "CD"
subopt "CCC" = "CCD"
subopt x = x

