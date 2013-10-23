-- Some helper functions to make working with HaXml more fluent

module HaXmlHelper
	( Node, Nodes, Filter
	, parseXml
	, structure, unroll
	, allTags, allText, innerText, innerTexts, texts
	, rootElementName
	, matchingText, matchingAnyText, exactAttribute) where

import Text.XML.HaXml
import Text.XML.HaXml.Types
import Text.XML.HaXml.Parse
import Text.XML.HaXml.Posn (Posn, noPos)
import Text.XML.HaXml.Combinators

type Node = Content Posn
type Nodes = [Content Posn]
type Filter = Node -> Nodes -- (Node -> Nodes) == (CFilter Posn)

-- Given a list of strings, keep nodes that exactly match any in the list
matchingAnyText :: [String] -> Filter
matchingAnyText [] = none
matchingAnyText (t:ts) = (matchingText t) |>| (matchingAnyText ts)

-- Keep nodes with the exact named attribute value
exactAttribute :: String -> String -> Filter
exactAttribute name value = attrval (N name, AttValue [Left value])

-- Given a filter and a source document, concat all matching inner-texts into one string
allText :: Filter -> Node -> String
allText filter doc = innerText (filter doc)

-- Given a filter and a source document, return a list of all matching inner-texts.
texts :: Filter -> Nodes -> [String]
texts filter doc = fmap (innerText . filter) doc

-- Given a filter and a set of nodes, return all sets of filter results
unroll :: (Node -> a) -> Nodes -> [a]
unroll filter nodes = fmap (filter) nodes

-- Take a set of nodes and return an XML string of their contents
structure :: Verbatim a => [a] -> [Char] -- Nodes -> String
structure nodes = concatMap (verbatim) nodes

-- Filter nodes based on exact text content
matchingText :: String -> Filter
matchingText text = ifTxt (\s -> if (s == text) then keep else none) none

-- Parse a DDEX XML string into nodes
parseXml :: String -> Nodes
parseXml fileName = docContent $ xmlParse ("error in " ++ fileName) fileName

-- All matching tags in the document
allTags :: String -> Filter
allTags = deep . tag

-- Concatenate all text (without elements) from the given nodes
innerText :: Nodes -> String
innerText nodes = concatMap verbatim (map (keep /> txt) nodes)

-- Return text nodes contents as a list of strings
innerTexts :: Nodes -> [String]
innerTexts nodes = map verbatim (map (keep /> txt) nodes)


-- Get the name of the root element
rootElementName :: Document a -> String
rootElementName (Document _ _ elem _ ) = elemName elem
	where
		elemName (Elem (N n) _ _) = n
		elemName (Elem (QN ns n) _ _) = n

-- Select New release message from an XML document
docContent :: Document Posn -> Nodes
docContent (Document _ _ elem _ ) = tag "ernm:NewReleaseMessage" $ CElem elem noPos -- todo: generalise this!