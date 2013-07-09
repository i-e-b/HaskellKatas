
-- requires: cabal install haxml
-- xml experiments

import Text.XML.HaXml
import Text.XML.HaXml.Types
import Text.XML.HaXml.Parse
import Text.XML.HaXml.Posn

exampleFile = "example.xml"

main = readFile exampleFile >>= putStrLn . processXml .(xmlParse ("error in " ++ exampleFile))

-- really simple test: get the name of the root element
processXml :: Document a -> String
processXml (Document _ _ elem _ ) = elemName elem
	where
		elemName (Elem (N n) _ _) = n
		elemName (Elem (QN ns n) _ _) = n
--

