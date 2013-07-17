
-- requires: cabal install haxml
-- xml experiments

import Text.XML.HaXml
import Text.XML.HaXml.Types
import Text.XML.HaXml.Parse
import Text.XML.HaXml.Posn (Posn, noPos)
import Text.XML.HaXml.Combinators

import Control.Applicative

type Node = Content Posn
type Nodes = [Content Posn]

exampleFile = "example.xml"

main = readFile exampleFile >>= putStrLn . testFunc . docContent. xmlParse ("error in " ++ exampleFile)

{-- count deal tags
testFunc doc = show $ foldr (+) 0 (allMatches doc)
	where
		allMatches doc = fmap (length . allDeals) doc
		-}

-- find first preference sender id
testFunc doc = show $ foldr (++) "" (allMatches doc)
	where
		allMatches doc = fmap (verbatim . senderId) doc
		

--doc <$> docContent <*> senderId <*> verbatim
--verbatim $ fmap senderId (docContent doc)


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

-- Node element of a document, as Content
docContent :: Document Posn -> Nodes
docContent (Document _ _ elem _ ) = tag "ernm:NewReleaseMessage" $ CElem elem noPos

{- - DDEX Specific - -}
allDeals :: Node -> Nodes
allDeals = allTags "Deal"

senderId :: Node -> Nodes
senderId = ((allTags "MessageHeader" /> tag "SentOnBehalfOf" /> tag "PartyId")
	|>| (allTags "MessageHeader" /> tag "MessageSender" /> tag "PartyId"))


{- - Utilities - -}
-- Convert [Content] to a printable String, with a default if not found
contentToStringDefault :: String -> [Content Posn] -> String
contentToStringDefault msg [] = msg
contentToStringDefault _ x = contentToString x

-- Convert [Content] to a printable string, taking care to unescape it.
contentToString :: [Content Posn] -> String
contentToString = 
    concatMap procContent
    where procContent x = 
              verbatim $ keep /> txt $ CElem (unesc (fakeElem x)) noPos

          fakeElem :: Content Posn -> Element Posn
          fakeElem x = Elem (N "fake") [] [x]

          unesc :: Element Posn -> Element Posn
          unesc = xmlUnEscape stdXmlEscaper
