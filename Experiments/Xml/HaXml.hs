
-- requires: cabal install haxml
-- xml experiments

import Text.XML.HaXml
import Text.XML.HaXml.Types
import Text.XML.HaXml.Parse
import Text.XML.HaXml.Posn (Posn, noPos)
import Text.XML.HaXml.Combinators

import Control.Applicative

-- (Node -> Nodes) == (CFilter Posn)
type Node = Content Posn
type Nodes = [Content Posn]

data Product = Product {sender::String, title::String} deriving (Show)

exampleFile = "example.xml"

main = readFile exampleFile >>= putStrLn . show . readProduct . parseXml

checkProduct :: IO ()
checkProduct = do
	doc <- readFile exampleFile 
	putStrLn . show $ (struct {-. releaseTitle-} . productRelease <$> (parseXml doc))

struct nodes = concatMap (verbatim) nodes

-- Parse a DDEX XML string into nodes
parseXml :: String -> Nodes
parseXml fileName = docContent $ xmlParse ("error in " ++ fileName) fileName

readProduct :: Nodes -> Product
readProduct n = Product {sender=senderString n, title=productTitle n}

{-- count deal tags
testFunc doc = show $ foldr (+) 0 (allMatches doc)
	where
		allMatches doc = fmap (length . allDeals) doc
		-}

-- find first preference sender id
senderString :: Nodes -> String
senderString doc = concat (innerText . senderId <$> doc)

-- product type release title: ReleaseList/Release(/ReleaseType = Album|Bundle)/ReferenceTitle/TitleText -> inner text
productTitle :: Nodes -> String
productTitle doc = concat (innerText . productRelease <$> doc)

{- - General - -}
-- Concatenate all text (without elements) from the given nodes
innerText :: Nodes -> String
innerText nodes = concatMap verbatim (map (keep /> txt) nodes)

-- All matching tags in the document
allTags :: String -> CFilter i
allTags = deep . tag

-- Get the name of the root element
rootElementName :: Document a -> String
rootElementName (Document _ _ elem _ ) = elemName elem
	where
		elemName (Elem (N n) _ _) = n
		elemName (Elem (QN ns n) _ _) = n

-- Select New release message from an XML document
docContent :: Document Posn -> Nodes
docContent (Document _ _ elem _ ) = tag "ernm:NewReleaseMessage" $ CElem elem noPos

{- - DDEX Specific - -}
allDeals :: Node -> Nodes
allDeals = allTags "Deal"

senderId :: Node -> Nodes
senderId = (allTags "MessageHeader" /> tag "SentOnBehalfOf" /> tag "PartyId")
	|>| (allTags "MessageHeader" /> tag "MessageSender" /> tag "PartyId") -- `|>|` means output right only if no left.

releaseTitle :: Nodes -> Nodes
releaseTitle = concatMap (allTags "ReferenceTitle" /> tag "TitleText")

-- maybe also try `</` rather than `with`
productRelease :: Node -> Nodes
productRelease = (allTags "ReleaseList" /> tag "Release") `with` (tag "ReleaseType" {-/> literal "Album"-})
-- allTags doesn't actually filter on releasetype = album
-- tag "ReleaseType" returns nothing.


