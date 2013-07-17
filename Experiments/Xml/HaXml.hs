
-- requires: cabal install haxml
-- xml experiments

import Text.XML.HaXml
import Text.XML.HaXml.Types
import Text.XML.HaXml.Parse
import qualified Text.XML.HaXml.Pretty as Print
import Text.XML.HaXml.Posn
import Text.XML.HaXml.Combinators

type Root = Content Posn

exampleFile = "example.xml"

main = readFile exampleFile >>= putStrLn . testFunc . xmlParse ("error in " ++ exampleFile)

testFunc = senderId . docContent


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

-- Root element of a document, as Content
docContent :: Document Posn -> Root
docContent (Document _ _ elem _ ) = CElem elem noPos

{- - DDEX Specific - -}
howManyDeals :: Root -> String
howManyDeals doc = show . length $ allTags "Deal" doc

senderId :: Root -> String
senderId doc = verbatim (tag "ernm:NewReleaseMessage" /> tag "MessageHeader" /> tag "MessageSender" /> tag "PartyId" $ doc)


{- - Utilities - -}
{- | Convert [Content] to a printable String, with a default if the 
passed-in [Content] is [], signifying a lack of a match. -}
contentToStringDefault :: String -> [Content Posn] -> String
contentToStringDefault msg [] = msg
contentToStringDefault _ x = contentToString x

{- | Convert [Content] to a printable string, taking care to unescape it.

An implementation without unescaping would simply be:

> contentToString = concatMap (show . content)

Because HaXml's unescaping only works on Elements, we must make sure that
whatever Content we have is wrapped in an Element, then use txt to
pull the insides back out. -}
contentToString :: [Content Posn] -> String
contentToString = 
    concatMap procContent
    where procContent x = 
              verbatim $ keep /> txt $ CElem (unesc (fakeElem x)) noPos

          fakeElem :: Content Posn -> Element Posn
          fakeElem x = Elem (N "fake") [] [x]

          unesc :: Element Posn -> Element Posn
          unesc = xmlUnEscape stdXmlEscaper
