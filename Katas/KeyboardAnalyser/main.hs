import Data.List
import Data.Char
import Data.Maybe
{- 
	Keyboard analyser in Haskell.
	Purpose is to take a description of keyboard layout
	and a test script, then output an analysed version of the test script

	Output is coded:
	  left keys are lowercase
	  right keys are upper case
	  collision groups (to be avoided) start with '[' and end with ']'
	  double letters (like "ll", "oo") are stripped.
-}

main = do
	str <- getContents
	putStrLn . analyseString $ str
	
data Side = L | R deriving (Show, Eq)
type Column = Integer
type KeyPosition = (Char, Column, Side)

data Statistics = Statistics { left::Int, right::Int, collisions::Int } deriving (Show)

type Analysis = [[KeyPosition]]
type Keyboard = [KeyPosition]
{-
 OLV  IYX
THE_  .DNA
QPWU  MGJR
 ZKF  SCB
           
abcdefghijklmnopqrstuvwxyz._
abcdefghijklmnopqrstuvwxyz._
 
-}
theKeyboard =
	[             ('O', 1, L), ('L', 2, L), ('V', 3, L),     ('I', 4, R), ('Y', 5, R), ('X', 6, R)
	,('T', 0, L), ('H', 1, L), ('E', 2, L), (' ', 3, L),     ('.', 4, R), ('D', 5, R), ('N', 6, R), ('A', 7, R)
	,('Q', 0, L), ('P', 1, L), ('W', 2, L), ('U', 3, L),     ('M', 4, R), ('G', 5, R), ('J', 6, R), ('R', 7, R)
	,             ('Z', 1, L), ('K', 2, L), ('F', 3, L),     ('S', 4, R), ('C', 5, R), ('B', 6, R)
	,(',', 4, R) ] -- period and comma on same key

{- Targets:
   * Minimise same finger
   * Ignore alternation
   * Prefer 'finger rolls' for common bi- and tri- graphs
-}


analyseString x = concat
	[printAnalysis . (analyse theKeyboard) $ x 
	, "\r\n"
	, printStatistics . statistics . (analyse theKeyboard) $ x]

printAnalysis :: Analysis -> String
printAnalysis anz = concat (map inner anz)
	where
		col str x = if (length x) > 1 then str else ""
		inner x = concat [ f x | f <- [col "[", caseLR, col "]"]]
		caseLR  = map (\(chr, col, side) -> if (side == L) then (toLower chr) else (toUpper chr))

printStatistics :: Statistics -> String
printStatistics x = "Left keys: " ++ (lefts x) ++ "; Right keys: " ++ (rights x) ++ "; Collisions: " ++ (collides x)
	where
		lefts (Statistics ls rs cl) = show ls
		rights (Statistics ls rs cl) = show rs
		collides (Statistics ls rs cl) = show cl
		
-- Analyse a string against a keyboard.
analyse :: Keyboard -> String -> Analysis
analyse k s = groupedKeys . (mapKeys k) $ singles s

-- Get usage statistics
statistics :: Analysis -> Statistics
statistics = (foldl (addstat) (Statistics 0 0 0)) . statlist
	where
		addstat (Statistics lA rA cA) (Statistics lB rB cB) = Statistics (lA + lB) (rA + rB) (cA + cB)

statlist :: [[KeyPosition]] -> [Statistics]
statlist x = (concat [map (leftOrRight) y | y <- x]) ++ (map collides x)
	where
		leftOrRight keypos = Statistics (isLeftSide keypos) (isRightSide keypos) 0
		collides grp = Statistics 0 0 ((length grp) - 1)

isLeftSide :: KeyPosition -> Int
isLeftSide (_, _, side) = if (side == L) then 1 else 0
isRightSide x = 1 - (isLeftSide x)
		
-- string with double-letters removed
singles :: String -> String
singles str = map (\x -> x !! 0) (group str)

-- Group keys by column (any groups length > 1 are undesirable)
collision (kA, cA, sA) (kB, cB, sB) = cA == cB
groupedKeys :: [KeyPosition] -> [[KeyPosition]]
groupedKeys = groupBy collision

-- True if the KeyPosition is that of the char provided
matchChar :: Char -> KeyPosition -> Bool
matchChar char (k, col, side) = (toUpper k) == (toUpper char)

-- Take a keyboard and a string, output a list of key positions
mapKeys :: Keyboard -> [Char] -> [KeyPosition]
mapKeys keyb str = map fromJust $ filter (isJust) [ find (matchChar x) keyb | x <- str]








