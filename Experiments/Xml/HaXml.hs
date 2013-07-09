
-- requires: cabal install haxml
-- xml experiments

import Text.XML.HaXml
import Text.XML.HaXml.Types
import Text.XML.HaXml.Parse
import Text.XML.HaXml.Posn

exampleFile = "example.xml"

main = readFile exampleFile >>= putStrLn . testFunc .(xmlParse ("error in " ++ exampleFile))

testFunc = howManyDeals -- should be 12

-- really simple test: get the name of the root element
rootElementName :: Document a -> String
rootElementName (Document _ _ elem _ ) = elemName elem
	where
		elemName (Elem (N n) _ _) = n
		elemName (Elem (QN ns n) _ _) = n
--

howManyDeals :: Document Posn -> String
howManyDeals doc = show . length $ allTags "Deal" (rootElemContent doc)
	where
		allTags = deep . tag

rootElemContent :: Document Posn -> Content Posn
rootElemContent (Document _ _ elem _ ) = CElem elem noPos

