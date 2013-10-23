
-- requires: cabal install haxml
-- Parsing DDEX documents in Haskell

import Text.XML.HaXml
import Text.XML.HaXml.Types
import Text.XML.HaXml.Parse
import Text.XML.HaXml.Posn (Posn, noPos)
import Text.XML.HaXml.Combinators

import Control.Applicative

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
checkPath :: String -> (Node -> Nodes) -> IO ()
checkPath file filter = do
	doc <- readFile file 
	putStrLn . show $ (structure . filter <$> (parseXml doc))
	
dig :: (Show a) => String -> (Node -> a) -> IO ()
dig file filter = do
	doc <- readFile file 
	putStrLn . show $ (filter <$> (parseXml doc))

exampleFile = "example.xml"
main = readFile exampleFile >>= putStrLn . show . readProduct . parseXml

-- Populate a product record from a ddex NewReleaseMessage
readProduct :: Nodes -> Product
readProduct n = Product
	{ sender = senderString n
	, product_title = productTitle n
	, tracks = map (readTrack) (trackCodes n)
	}

-- return the Resource code and Release code for all track-level releases
trackCodes :: Nodes -> [(String, String)]
trackCodes n =
	let eachTrackRef node = map (releaseReference) (trackReleases node)
	    allReleaseRefs nodes = concat (unroll (eachTrackRef) nodes)
	in  map (\releaseCode -> (releaseCode, "")) (allReleaseRefs n)

-- given the release and resource codes of a track, populate a track record
readTrack :: (String, String) -> Track
readTrack rsrc =  Track
	{ disc = 1
	, track = 1
	, track_title = (fst rsrc)
	, territories = []
	}

-- find first preference sender id
senderString :: Nodes -> String
senderString = allText senderId

-- product type release title: ReleaseList/Release(/ReleaseType = Album|Bundle)/ReferenceTitle/TitleText -> inner text
productTitle :: Nodes -> String
productTitle = allText (releaseTitle . productRelease)



