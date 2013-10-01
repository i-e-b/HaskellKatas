-- simple polynomial guessing
-- Translation from Python from http://jliszka.github.io/2013/09/24/more-backwards-functions-unevaluating-polynomials.html

-- use like
--  > evalPoly (8, [1,3])
-- (8, 25)
--  > unevalPoly (8, 25)
-- (8, [1, 3])
-- This works by treating the polynomial as a base term

-- given x and a list of positive polynomial terms, evaluate a result
evalPoly :: (Int, [Int]) -> (Int, Int)
evalPoly x coeff =
    let reduce[]    = 0
        reduce(h:t) = x * (reduce t) + h
    in  (x, reduce coeff)

-- given the result of evalPoly, try to guess the source coefficients
unevalPoly :: (Int, Int) -> (Int, [Int])
unevalPoly (x,y) = 
    let expand 0  = []
        expand y' = (y' `mod` x) : (expand (y' `div` x)) 
    in  (x, expand y)

