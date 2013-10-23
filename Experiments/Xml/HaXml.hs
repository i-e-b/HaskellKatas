
-- requires: cabal install haxml
-- Parsing DDEX documents in Haskell

import Text.XML.HaXml
import Text.XML.HaXml.Types
import Text.XML.HaXml.Parse
import Text.XML.HaXml.Posn (Posn, noPos)
import Text.XML.HaXml.Combinators

import HaXmlHelper
import DdexParsing

data Track = Track
	{ disc :: Int
	, track :: Int
	, track_title :: String
	, territories :: [String]
	} deriving (Show)

data Product = Product 
	{ sender::String
	, product_title::String
	, tracks::[Track]
	} deriving (Show)

-- give a file name and a filter, will print resulting structure	
showFilter :: (Show a) => String -> (Node -> a) -> IO ()
showFilter file filter = do
	doc <- readFile file 
	putStrLn . show $ (fmap (filter) (parseXml doc))

exampleFile = "example.xml"
main = showFilter exampleFile readProduct

-- Populate a product record from a ddex NewReleaseMessage
readProduct :: Node -> Product
readProduct n = Product
	{ sender = senderString n
	, product_title = productTitle n
	, tracks = map (readTrack) (trackCodes n)
	}

-- return the Resource code and Release code for all track-level releases
trackCodes :: Node -> [(String, String)]
trackCodes n =
	let refs = map (releaseReference) (trackReleases n)
	in  map (\releaseCode -> (releaseCode, "")) (refs)

-- given the release and resource codes of a track, populate a track record
readTrack :: (String, String) -> Track
readTrack rsrc =  Track
	{ disc = 1
	, track = 1
	, track_title = (fst rsrc)
	, territories = []
	}

-- find first preference sender id
senderString :: Node -> String
senderString = innerText . senderId

-- product type release title: ReleaseList/Release(/ReleaseType = Album|Bundle)/ReferenceTitle/TitleText -> inner text
productTitle :: Node -> String
productTitle = innerText . releaseTitle . productRelease



