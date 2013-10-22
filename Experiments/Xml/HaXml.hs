
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

data Product = Product {sender::String, title::String} deriving (Show)

-- give a file name and a filter, will print resulting structure
checkPath :: String -> (Node -> Nodes) -> IO ()
checkPath file filter = do
	doc <- readFile file 
	putStrLn . show $ (structure . filter <$> (parseXml doc))

exampleFile = "example.xml"
main = readFile exampleFile >>= putStrLn . show . readProduct . parseXml

-- Populate a product record from a ddex NewReleaseMessage
readProduct :: Nodes -> Product
readProduct n = Product {sender=senderString n, title=productTitle n}

-- find first preference sender id
senderString :: Nodes -> String
senderString = allText senderId

-- product type release title: ReleaseList/Release(/ReleaseType = Album|Bundle)/ReferenceTitle/TitleText -> inner text
productTitle :: Nodes -> String
productTitle = allText (releaseTitle . productRelease)



