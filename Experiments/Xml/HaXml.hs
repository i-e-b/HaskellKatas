
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

exampleFile = "example.xml"

main = readFile exampleFile >>= putStrLn . testFunc . parseXml

-- Parse a DDEX XML string into nodes
parseXml :: String -> Nodes
parseXml fileName = docContent $ xmlParse ("error in " ++ fileName) fileName

-- Concatenate all text (without elements) from the given nodes
innerText :: Nodes -> String
innerText nodes = concat $ map verbatim (map (keep /> txt) nodes)

{-- count deal tags
testFunc doc = show $ foldr (+) 0 (allMatches doc)
	where
		allMatches doc = fmap (length . allDeals) doc
		-}

-- find first preference sender id
testFunc :: Nodes -> String
testFunc doc = concat (innerText . senderId <$> doc)

{- - General - -}
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
senderId = ((allTags "MessageHeader" /> tag "SentOnBehalfOf" /> tag "PartyId")
	|>| (allTags "MessageHeader" /> tag "MessageSender" /> tag "PartyId"))


