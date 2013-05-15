
main = do
	putStrLn "Give me a natural number"
	num <- getLine
	putStrLn (romanise . read $ num)
	
romanise :: Int -> String
romanise x =
	rom x ""

rom :: Int -> String -> String
rom 0 str = str
rom i str 
	| i >= 1000 = rom (i - 1000) (str:"M")
	| i >= 500 = rom (i - 500) (str:"D")
	| i >= 100 = rom (i - 100) (str:"C")
	| i >= 50 = rom (i - 50) (str:"L")
	| i >= 10 = rom (i - 10) (str:"X")
	| i >= 5 = rom (i - 5) (str:"V")
	| otherwise = rom (i - 1) (str:"I")
