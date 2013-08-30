-- powerset of a list
-- that is, all combinations of filtering the list

import Control.Monad

powerset :: [a] -> [[a]]
powerset = filterM (const [True, False])

example = powerset "abc" -- > ["abc","ab","ac","a","bc","b","c",""] 

