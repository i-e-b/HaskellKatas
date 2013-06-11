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

analyseString x = printAnalysis . (analyse theKeyboard) $ x
	
data Side = L | R deriving (Show, Eq)
type Column = Integer
type KeyPosition = (Char, Column, Side)

data Statistics = Statistics { left::Int, right::Int, collisions::Int } deriving (Show)

type Analysis = [[KeyPosition]]
type Keyboard = [KeyPosition]
theKeyboard =
	[             ('Q', 1, L), ('C', 2, L), ('K', 3, L),     ('D', 4, R), ('L', 5, R), ('.', 6, R)
	,('A', 0, L), ('R', 1, L), ('S', 2, L), ('T', 3, L),     ('H', 4, R), ('N', 5, R), ('E', 6, R), ('I', 7, R)
	,('B', 0, L), ('F', 1, L), ('P', 2, L), ('G', 3, L),     (' ', 4, R), ('J', 5, R), ('O', 6, R), ('Y', 7, R)
	,             ('X', 1, L), ('U', 2, L), ('M', 3, L),     ('W', 4, R), ('V', 5, R), ('Z', 6, R)]

{- Targets:
   * Minimise same finger
   * Ignore alternation
   * Prefer 'finger rolls' for common bi- and tri- graphs
-}


printAnalysis :: Analysis -> String
printAnalysis anz = concat (map inner anz)
	where
		dEnd x = if (length x) > 1 then "]" else ""
		dStart x = if (length x) > 1 then "[" else ""
		inner x = (dStart x) ++ (map caseLR x) ++ dEnd x

caseLR :: KeyPosition -> Char
caseLR (chr, col, side) = if (side == L) then (toLower chr) else (toUpper chr)
		
-- Analyse a string against a keyboard.
analyse :: Keyboard -> String -> Analysis
analyse k s = groupedKeys . (mapKeys k) $ singles s

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








