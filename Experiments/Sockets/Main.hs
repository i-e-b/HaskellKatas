import Network
import Data.Char (toLower)
import System.IO (hGetLine,hClose,hPutStrLn,hSetBuffering,BufferMode(..),Handle,stdout)
 
port = 8001 -- a nice port number
 
main = withSocketsDo $ do -- enable sockets under windows
	putStrLn "Welcome to One-Way Chat version 1.0"
	hSetBuffering stdout NoBuffering -- fix buffering under windows
	input <- untilM   -- get input of 'c' or 's'
		(\x -> (not $ null x) && toLower (head x) `elem` "cs")
		(putStr "Client or server? " >> getLine)
	(if (toLower (head input) == 'c') then client else server) -- start the client or server
--		`catch` (const $ putStrLn "Connection forced closed.")	
	putStrLn "Connection closed."
	putStrLn "Thanks for using One-Way Chat!" -- all done
 
-- reads in an ip address
readIp = putStr "Enter IP address: " >> getLine
 
-- monadic `until`
untilM p x = x >>= (\y -> if p y then return y else untilM p x) 
-- repeats two actions until either returns true
while2 x y = ifM x (return ()) $ ifM y (return ()) $ while2 x y 
-- monadic `if`
ifM p t f  = p >>= (\p' -> if p' then t else f)
 
-- client
client = do
	ip <- readIp
	putStrLn "Connecting..."
	h <- connectTo ip (PortNumber port)
	putStrLn $ "Connected to " ++ ip ++ ":" ++ show port
	hSetBuffering h LineBuffering
	while2 (send h) (receive h)
	hClose h
 
-- server
server = do
	sock <- listenOn (PortNumber port)
	putStrLn "Awaiting connection."
	(h,host,port) <- accept sock
	putStrLn $ "Received connection from " ++ host ++ ":" ++ show port
	hSetBuffering h LineBuffering
	while2 (receive h) (send h)
	hClose h
	sClose sock
 
-- sending
send h = do
	putStr "Send (empty to close): "
	input <- getLine
	hPutStrLn h input
	return $ null input
 
-- receiving
receive h = do
	putStr "Receiving: "
	input <- hGetLine h
	putStrLn input
	return $ null input
