
-- run in the ghci interpreter

-- Split a string by spaces, fold resulting list by RPN rules, take the top item as result
solveRPN :: String -> Double
solveRPN = head . foldl operand [] . words
	where
		-- binary operators
		operand (b:a:rest) "+" = (a + b):rest
		operand (b:a:rest) "-" = (a - b):rest
		operand (b:a:rest) "*" = (a * b):rest
		operand (b:a:rest) "/" = (a / b):rest
		operand (b:a:rest) "^" = (a ** b):rest
		-- unary operators
		operand (a:rest) "ln" = (log a):rest
		-- whole stack operators
		operand rest "sum" = [sum rest]
		-- numbers
		operand rest num = read num:rest
