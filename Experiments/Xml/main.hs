-- Xml experiments in Haskell
-- Trying a few queries against a DDEX document.
-- For DDEX spec see http://ddex.net/dd/DDEX-ERN-341-DD/
-- Much cribbed from https://github.com/petermarks/hoodlums-sessions/blob/master/xml.hs

import Prelude hiding (elem)
import Control.Applicative
import Control.Monad
import Data.DList -- Differences lists: a list-like type supporting O(1) append. This is particularly useful for efficient logging and pretty printing, (e.g. with the Writer monad), where list append quickly becomes too expensive.
import Data.Maybe
import Text.XML.Light
import Text.XML.Light.Cursor

-- Load sample document
load :: IO Element
load = fromJust . parseXMLDoc <$> readFile "example.xml"

type Trans a b = a -> DList b

-- Pretty print lists of content
pp :: [Content] -> IO ()
pp = mapM_ $ putStrLn . ppContent


children :: Trans Cursor Cursor
children (current -> Elem e) = 
  fromList $ fmap fromElement $ elChildren e
children _ = mzero

descendants :: Trans Cursor Cursor
descendants = children >=> (\c -> return c `mplus` descendants c)

hasTag :: QName -> Trans Cursor Cursor
hasTag n c@(current -> Elem e) | n == elName e = return c
hasTag _ _ = mzero

getText :: Trans Cursor String
getText (current -> Elem e) = return $ strContent e
getText _ = mzero

elem :: QName -> Trans a Content -> Trans a Content
elem n c = return . Elem . node n . toList . c

text :: Trans String Content
text s = return . Text $ CData CDataText s Nothing
