
-- Some helper functions to make working with HaXml more fluent
module HaXmlHelper
	( Node, Nodes
	, parseXml
	, structure, unroll
	, allTags, allText, innerText, texts
	, rootElementName
	, matchingText, matchingAnyText) where

import Text.XML.HaXml
import Text.XML.HaXml.Types
import Text.XML.HaXml.Parse
import Text.XML.HaXml.Posn (Posn, noPos)
import Text.XML.HaXml.Combinators
import Control.Applicative ( (<$>) )

-- (Node -> Nodes) == (CFilter Posn)
type Node = Content Posn
type Nodes = [Content Posn]

-- Given a list of strings, keep nodes that exactly match any in the list
matchingAnyText :: [String] -> Node -> Nodes
matchingAnyText [] = none
matchingAnyText (t:ts) = (matchingText t) |>| (matchingAnyText ts)

-- Given a filter and a source document, concat all matching inner-texts into one string
allText :: (Node -> Nodes) -> Nodes -> String
allText filter doc = concat (innerText . filter <$> doc)

-- Given a filter and a source document, return a list of all matching inner-texts.
texts :: (Node -> Nodes) -> Nodes -> [String]
texts filter doc = innerText . filter <$> doc

-- Given a filter and a set of nodes, return all sets of filter results
unroll :: (Node -> a) -> Nodes -> [a]
unroll filter nodes = filter <$> nodes

-- Take a set of nodes and return an XML string of their contents
structure :: Verbatim a => [a] -> [Char] -- Nodes -> String
structure nodes = concatMap (verbatim) nodes

-- Filter nodes based on exact text content
matchingText :: String -> Node -> Nodes
matchingText text = ifTxt (\s -> if (s == text) then keep else none) none

-- Parse a DDEX XML string into nodes
parseXml :: String -> Nodes
parseXml fileName = docContent $ xmlParse ("error in " ++ fileName) fileName

-- All matching tags in the document
allTags :: String -> Node -> Nodes
allTags = deep . tag

-- Concatenate all text (without elements) from the given nodes
innerText :: Nodes -> String
innerText nodes = concatMap verbatim (map (keep /> txt) nodes)

-- Get the name of the root element
rootElementName :: Document a -> String
rootElementName (Document _ _ elem _ ) = elemName elem
	where
		elemName (Elem (N n) _ _) = n
		elemName (Elem (QN ns n) _ _) = n

-- Select New release message from an XML document
docContent :: Document Posn -> Nodes
docContent (Document _ _ elem _ ) = tag "ernm:NewReleaseMessage" $ CElem elem noPos -- todo: generalise this!