import Data.Time.Calendar

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

main :: IO ()
main = do
    year <- intInput "year"
    width <- intInput "characters wide"
    putStrLn $ calendarFor year width

intInput :: String -> IO Int
intInput msg = putStrLn msg >> getLine >>= \s -> return (read s :: Int)

calendarFor :: Int -> Int -> String
calendarFor year width = "hello world"

