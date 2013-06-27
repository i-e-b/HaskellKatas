import Data.List

main = do
	putStrLn "Give me a natural number"
	numinp <- getLine
	let n = read numinp :: Int
	putStrLn . romanise $ n
--

-- Mapping of descending integer values and their RN equivalents.
romanMapping :: [(Int, String)]
romanMapping = 
	[ (1000, "M"),  (500, "D"),  (100, "C"),  (90, "XC")
	, (50,   "L"),  (40,  "XL"), (10,  "X"),  (9,  "IX")
	, (5,    "V"),  (4,   "IV"), (1,   "I")]

romanise :: Int -> String
romanise = numerals romanMapping ""

-- decrement the input number by each reducing scale while the input is >= the scale
-- [Scale] -> starting string -> input number -> output string
numerals :: [(Int,String)] -> String -> Int -> String
numerals ((scale, chr):xs) s i
	| i >= scale = numerals ((scale,chr):xs) (s ++ chr) (i - scale)
	| otherwise  = numerals xs s i
numerals [] s i = s
