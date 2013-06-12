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
	
{-
 JPY   BM.
ANDI   SCLZ
OTHE   G_UR
 XKQ   VWF 
-}
theKeyboard =
	[             ('J', 1, L), ('P', 2, L), ('Y', 3, L),     ('B', 4, R), ('M', 5, R), ('.', 6, R)
	,('A', 0, L), ('N', 1, L), ('D', 2, L), ('I', 3, L),     ('S', 4, R), ('C', 5, R), ('L', 6, R), ('Z', 7, R)
	,('O', 0, L), ('T', 1, L), ('H', 2, L), ('E', 3, L),     ('G', 4, R), (' ', 5, R), ('U', 6, R), ('R', 7, R)
	,             ('X', 1, L), ('K', 2, L), ('Q', 3, L),     ('V', 4, R), ('W', 5, R), ('F', 6, R)
	,(',', 6, R) ] -- period and comma on same key

{- 
For technical writing:
1's:  ET,AIS,LNOR,DPU,BCFGHMV,JKQWXZY
2's:  TH ER ON AN RE HE IN ED ND
3's:  THE AND THA ENT ION TIO FOR NDE HAS NCE

Start of word: T O A W B C D S F M R
End of word:   E S T D N R Y F L O G H A K

Targets:
   * Minimise same finger
   * Ignore alternation
   * Prefer 'finger rolls' for common bi- and tri- graphs
-}

data Side = L | R deriving (Show, Eq)
type Column = Integer
type KeyPosition = (Char, Column, Side)

data Statistics = Statistics { left::Int, right::Int, collisions::Int } deriving (Show)

type Analysis = [[KeyPosition]]
type Keyboard = [KeyPosition]

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








