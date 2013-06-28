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
Targets:
   * Minimise same finger
   * Ignore alternation
   * Prefer 'finger rolls' for common bi- and tri- graphs
   
For technical writing:
1's:  ET,AIS,LNOR,DPU,BCFGHMV,JKQWXZY
2's:  TH ER ON AN RE HE IN ED ND
3's:  THE AND THA ENT ION TIO FOR NDE HAS NCE

Start of word: T O A W B C D S F M R
End of word:   E S T D N R Y F L O G H A K

"PJYMB.ANZIDSLCOH ETGURKXQWVF" --> low collision
 PJY   MB.
ANZI   DSLC
OH_E   TGUR
 KXQ   WVF 
;
"MP.CXFZTHE UJSRLIODNAGWKQVYB" --> good rolls
 MP.   CXF
ZTHE   _UJS
RLIO   DNAG
 WKQ   VYB
-}
allKeyChars = "MP.CXFZTHE UJSRLIODNAGWKQVYB"
theKeyboard = buildKeyboard allKeyChars blankLayout
	where
		buildKeyboard (letter:ls) (key:ks) = (setKey letter key) : buildKeyboard ls ks
		buildKeyboard _ _ = []
		setKey newletter (oldletter, column, side) = (newletter, column, side)
		blankLayout =
			[             ('?', 1, L), ('?', 2, L), ('?', 3, L),     ('?', 4, R), ('?', 5, R), ('?', 6, R)
			,('?', 0, L), ('?', 1, L), ('?', 2, L), ('?', 3, L),     ('?', 4, R), ('?', 5, R), ('?', 6, R), ('?', 7, R)
			,('?', 0, L), ('?', 1, L), ('?', 2, L), ('?', 3, L),     ('?', 4, R), ('?', 5, R), ('?', 6, R), ('?', 7, R)
			,             ('?', 1, L), ('?', 2, L), ('?', 3, L),     ('?', 4, R), ('?', 5, R), ('?', 6, R)]
	


data Side = L | R deriving (Show, Eq)
type Column = Int
type KeyPosition = (Char, Column, Side)

data Statistics = Statistics { left::Int, right::Int, collisions::Int, columns::[Int] } deriving (Show)

type Analysis = [[KeyPosition]]
type Keyboard = [KeyPosition]

analyseString x =
	let result = analyse theKeyboard x
	in concat [printAnalysis result
	   , "\r\n"
	   , printStatistics (statistics result)]

-- show the input coded for collisions and hand-use: lower case for left, upper for right, collisions wrapped in square brackets
printAnalysis :: Analysis -> String
printAnalysis anz = concat (map inner anz)
	where
		col str x = if (length x) > 1 then str else ""
		inner x = concat [ f x | f <- [col "[", caseLR, col "]"]]
		caseLR  = map (\(chr, col, side) -> if (side == L) then (toLower chr) else (toUpper chr))

-- Show finger use statistics
printStatistics :: Statistics -> String
printStatistics x = "Left keys: " ++ (show $ left x) ++ "; Right keys: " ++ (show $ right x) ++ "; Collisions: " ++ (show $ collisions x) ++ "; Freq: " ++ (frequencies x)
	where
		frequencies x = show $ take 8 (columns x)
		
-- Analyse a string against a keyboard.
analyse :: Keyboard -> String -> Analysis
analyse k s = groupedKeys . (mapKeys k) $ singles s

-- Get usage statistics
statistics :: Analysis -> Statistics
statistics ans = (foldl (addstat) (Statistics 0 0 0 [0,0..])) . statlist $ ans
	where
		addstat (Statistics lA rA cA clnA) (Statistics lB rB cB clnB) = Statistics (lA + lB) (rA + rB) (cA + cB) (sumPositions clnA clnB)

-- All the chosen keypositions, map left and right stats, column numbers and add collisions to end.
statlist :: [[KeyPosition]] -> [Statistics]
statlist x = (concat [map (leftOrRight) y | y <- x]) ++ (map collides x)
	where
		leftOrRight keypos = Statistics (isLeftSide keypos) (isRightSide keypos) 0 [(columnNumber keypos)]
		collides grp = Statistics 0 0 ((length grp) - 1) []
		columnNumber (_, col, _) = col

isLeftSide :: KeyPosition -> Int
isLeftSide (_, _, side) = if (side == L) then 1 else 0
isRightSide x = 1 - (isLeftSide x)

-- Input list of columns, output is columns each with count of hit,
-- so [1,2,1,4,6,6,7] becomes [0, 2, 1, 0, 1, 0, 2, 1, 0, 0, ...]
-- result is infinite list
sumPositions :: [Int] -> [Int] -> [Int]
sumPositions accum = addUp accum
	where
		addUp sums (x:xs) = addUp (incrl x sums) xs
		addUp sums [] = sums
		incrl a xs = (take (a) xs) ++ [((xs !! a) + 1)] ++ (drop (a+1) xs)

-- string with double-letters removed
singles :: String -> String
singles str = map (\x -> x !! 0) (group $ filter (\a -> elem a filterKeys) str)
	where
		filterKeys = allKeyChars ++ (map toLower allKeyChars)

-- Group keys by column (any groups length > 1 are undesirable)
groupedKeys :: [KeyPosition] -> [[KeyPosition]]
groupedKeys = 
	let collision (kA, cA, sA) (kB, cB, sB) = cA == cB
	in  groupBy collision

-- True if the KeyPosition is that of the char provided
matchChar :: Char -> KeyPosition -> Bool
matchChar char (k, col, side) = (toUpper k) == (toUpper char)

-- Take a keyboard and a string, output a list of key positions
mapKeys :: Keyboard -> [Char] -> [KeyPosition]
mapKeys keyb str = map fromJust $ filter (isJust) [ find (matchChar x) keyb | x <- str]








