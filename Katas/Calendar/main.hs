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

calendarFor :: Int -> Int -> String
calendarFor year width = unlines $ concatMap (monthToStrings year) [1..12]

-- given a year and a month, draw a calendar block
monthToStrings :: Int -> Int -> [String]
monthToStrings year month = weekLines $ weeksOfMonth year month
	where
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
	    rest = map (\day -> Cell day) [1..length]
	in leader ++ rest

-- list of month lengths for a year
monthLengths :: Int -> [Int]

monthLengths year = map (gregorianMonthLength (fromIntegral year)) [1..12]

-- given year and month, return the starting day of the week (mon = 1, sun =7)
monthStart :: Int -> Int -> Int
monthStart year month = justDay $ toWeekDate (fromGregorian (fromIntegral year) month 1)
	where justDay (_, _, day) = day

