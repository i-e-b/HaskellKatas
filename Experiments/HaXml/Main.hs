-- requires: cabal install haxml
-- Parsing DDEX documents in Haskell

import HaXmlHelper
import Text.XML.HaXml.Combinators
import DdexParsing

data Track = Track
	{ disc :: Int
	, track :: Int
	, isrc :: String
	, track_title :: String
	, territories :: [String]
	} deriving (Show)

data Product = Product 
	{ sender::String
	, product_title::String
	, tracks::[Track]
	} deriving (Show)

exampleFile = "example.xml"
main = showFilter exampleFile readProduct

-- give a file name and a filter, will print resulting structure	
showFilter :: (Show a) => String -> (Node -> a) -> IO ()
showFilter file filter = do
	doc <- readFile file 
	putStrLn . show $ (fmap (filter) (parseXml doc))

-- Populate a product record from a ddex NewReleaseMessage
readProduct :: Node -> Product
readProduct n = Product
	{ sender = senderString n
	, product_title = productTitle n
	, tracks = map (readTrack n) (trackCodes n)
	}

-- return the Resource code and Release code for all track-level releases
trackCodes :: Node -> [(String, String)]
trackCodes n = map (\release -> (releaseReference release, single $ releasePrimaryResources release)) (trackReleases n)

single = (!! 0)

-- given the release and resource codes of a track, populate a track record
readTrack :: Node -> (String, String) -> Track
readTrack doc (releaseId, rsrcId) = 
	let resource = (resourceById (rsrcId) doc) !! 0
	in  Track
		{ disc = 1
		, track = 1
		, isrc = trackISRC resource
		, track_title = releaseId ++ "/" ++ rsrcId
		, territories = downloadTerritories . (dealsForRelease releaseId) $ doc
		}

-- find first preference sender id
senderString :: Node -> String
senderString = innerText . senderId

-- product type release title: ReleaseList/Release(/ReleaseType = Album|Bundle)/ReferenceTitle/TitleText -> inner text
productTitle :: Node -> String
productTitle = allText (productRelease /> releaseTitle)



