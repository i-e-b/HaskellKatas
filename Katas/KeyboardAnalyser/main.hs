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

analyseString x = concat
	[printAnalysis . (analyse theKeyboard) $ x 
	, "\r\n"
	, printStatistics . statistics . (analyse theKeyboard) $ x]
	
data Side = L | R deriving (Show, Eq)
type Column = Integer
type KeyPosition = (Char, Column, Side)

data Statistics = Statistics { left::Int, right::Int, collisions::Int } deriving (Show)

type Analysis = [[KeyPosition]]
type Keyboard = [KeyPosition]
{-
 QCK  DL.
ARST  HNEI
GPB_  FJOY
 XUM  WVZ
-}
theKeyboard =
	[             ('Q', 1, L), ('C', 2, L), ('K', 3, L),     ('D', 4, R), ('L', 5, R), ('.', 6, R), (',', 6, R) -- dot and comma on same key
	,('A', 0, L), ('R', 1, L), ('S', 2, L), ('T', 3, L),     ('H', 4, R), ('N', 5, R), ('E', 6, R), ('I', 7, R)
	,('G', 0, L), ('P', 1, L), ('B', 2, L), (' ', 3, L),     ('F', 4, R), ('J', 5, R), ('O', 6, R), ('Y', 7, R)
	,             ('X', 1, L), ('U', 2, L), ('M', 3, L),     ('W', 4, R), ('V', 5, R), ('Z', 6, R)]

{- Targets:
   * Minimise same finger
   * Ignore alternation
   * Prefer 'finger rolls' for common bi- and tri- graphs
-}


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








