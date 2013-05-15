
main = do
	putStrLn "Give me a natural number"
	numinp <- getLine
	let n = read numinp :: Int1
	putStrLn (romanise n)

romanise :: Int -> String
romanise = rom "id est "

rom :: String -> Int -> String
rom s 0 = s
rom s i 
	| i >= 1000 = rom (s ++ "M") (i - 1000)
	| i >= 500 = rom (s ++ "D") (i - 500)
	| i >= 100 = rom (s ++ "C") (i - 100)
	| i >= 50 = rom (s ++ "L") (i - 50)
	| i >= 10 = rom (s ++ "X") (i - 10)
	| i >= 5 = rom (s ++ "V") (i - 5)
	| otherwise = rom (s ++ "I") (i - 1)
