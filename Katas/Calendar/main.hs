import Data.Time.Calendar
import Data.Time.Calendar.WeekDate (toWeekDate)
import Data.List

{-
 - A calendar printer.
 -
 - Given a year and a character width,
 - print out a tightly-packed calendar with months flowing right
 - and days wrapped per week
 -
 - Assume week starts on a monday
 -
 -}

data DayColumn = Blank | Cell Int deriving (Show)
type Week = [DayColumn]

monthNames = ["January","February","March","April","May","June","July","August","September","October","November","December"]
dayTitles = ["M", "T", "W", "T", "F", "S", "S"]

main :: IO ()
main = do
    year <- intInput "year"
    width <- intInput "characters wide"
    putStrLn $ calendarFor year width

intInput :: String -> IO Int
intInput msg = putStrLn msg >> getLine >>= \s -> return (read s :: Int)


-- given a year and a screen width, output a display string for the year calendar
calendarFor :: Int -> Int -> String
calendarFor year width = unlines $ grouped
	where
		perRow = width `div` 21
		grouped = map (\x -> clump year x perRow) [1,(perRow+1)..12]

		clump :: Int -> Int -> Int -> String
		clump year start row = unlines . (map (unwords)) . (groupIntoColumns row) $ blockm year [start..(start+row-1)]

		groupIntoColumns :: Int -> [[String]] -> [[String]]
		groupIntoColumns r = transpose . concat . (groupsOf r)

		blockm :: Int -> [Int] -> [[String]]
		blockm year months = [ monthToStrings year month | month <- months, month <= 12]

-- given a year and a month, draw a calendar block
monthToStrings :: Int -> Int -> [String]
monthToStrings year month = monthTitle : dayCols : (weekLines $ weeksOfMonth year month)
	where
		monthTitle = pad (replicate 21 ' ') (monthNames !! (month-1))
		dayCols = "  " ++ (intercalate "  " dayTitles)
		weekLines [] = []
		weekLines wks = concatMap (dayStr) (take 7 wks) : weekLines (drop 7 wks)

		dayStr (Cell x) = pad "   " (show x)
		dayStr _ = "   "

		pad str inp = (drop (length inp) str) ++ inp

-- given a year and a month, week rows for that month
weeksOfMonth :: Int -> Int -> Week
weeksOfMonth year month = -- only first week to start with
	let start = (monthStart year month) - 1
	    length = (monthLengths year) !! (month - 1)
	    leader = map (\_ -> Blank) [1..start]
	    run = map (\day -> Cell day) [1..length]
	    ending = map (\_ -> Blank) [(length+start)..42] -- 6 rows of 7 days, the max size of a month
	in leader ++ run ++ ending

-- list of month lengths for a year
monthLengths :: Int -> [Int]
monthLengths year = map (gregorianMonthLength (fromIntegral year)) [1..12]

-- given year and month, return the starting day of the week (mon = 1, sun =7)
monthStart :: Int -> Int -> Int
monthStart year month = justDay $ toWeekDate (fromGregorian (fromIntegral year) month 1)
	where justDay (_, _, day) = day

-- group list into fixed length sublists
groupsOf :: Int -> [a] -> [[a]]
groupsOf 0 _ = undefined
groupsOf _ [] = []
groupsOf n xs = take n xs : groupsOf n (drop n xs)

