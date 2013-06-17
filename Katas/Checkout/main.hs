{- Very basic version of the checkout kata

Item     Price     Offer
-------------------------------
 A       50        3 for 130
 B       30        2 for 45
 C       20
 D       15
 
Accept items in any order, apply matching offers

Sample assertions:

"" = 0
"A" = 50
"AB" = 80
"CDBA" = 115
"AA" = 100
"AAA" = 130
"AAABB" = 150
"ACAABCB" = 190

-}

import Data.List
import Test.HUnit

