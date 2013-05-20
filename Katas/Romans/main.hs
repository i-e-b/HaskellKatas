main = do
	putStrLn "Give me a natural number"
	numinp <- getLine
	let n = read numinp :: Int
	putStrLn . romanise $ n

romanise :: Int -> String
romanise = reverse . optimise . numerals ""

numerals :: String -> Int -> String
numerals s 0 = s
numerals s i 
	| i >= 1000 = numerals (s ++ "M") (i - 1000)
	| i >= 500 = numerals (s ++ "D") (i - 500)
	| i >= 100 = numerals (s ++ "C") (i - 100)
	| i >= 50 = numerals (s ++ "L") (i - 50)
	| i >= 10 = numerals (s ++ "X") (i - 10)
	| i >= 5 = numerals (s ++ "V") (i - 5)
	| otherwise = numerals (s ++ "I") (i - 1)

opt :: [Char] -> [Char] -> String
opt [] xss = xss
opt ('I':'I':'I':'I':xs) xss = opt xs ("vi" ++ xss)
opt ('I':'I':'I':xs) xss = opt xs ("vii" ++ xss)
opt ('X':'X':'X':'X':xs) xss = opt xs ("lx" ++ xss)
opt (x:xs) xss = opt xs (x : xss)

optimise :: String -> String
optimise x = opt x ""

