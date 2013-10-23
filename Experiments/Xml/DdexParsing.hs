
-- DDEX specific parsing functions.
-- Use this to decompose an XML message into a working record set.
module DdexParsing
	( allDeals, senderId, releaseTitle, productRelease, releaseReference, trackReleases) where

import Text.XML.HaXml
import Text.XML.HaXml.Types
import Text.XML.HaXml.Parse
import Text.XML.HaXml.Posn (Posn, noPos)
import Text.XML.HaXml.Combinators

import HaXmlHelper

-- All deals for all releases
allDeals :: Node -> Nodes
allDeals = allTags "Deal"

-- Sender ID, preferring "SentOnBehalfOf", returning "MessageSender" otherwise
senderId :: Node -> Nodes
senderId = (allTags "MessageHeader" /> tag "SentOnBehalfOf" /> tag "PartyId")
	|>| (allTags "MessageHeader" /> tag "MessageSender" /> tag "PartyId") -- `|>|` means output right only if no left.

-- Title of a release
releaseTitle :: Nodes -> Nodes
releaseTitle = concatMap (allTags "ReferenceTitle" /> tag "TitleText")

-- Reference codes for a release
releaseReference :: Node -> String
releaseReference n = innerText (allTags "ReleaseReference" n)

-- Return all releases with Product-level release types ("Bundle", "Single", "Album", "VideoAlbum", "VideoSingle")
productRelease :: Node -> Nodes
productRelease = (allTags "ReleaseList" /> tag "Release") `with` (allTags "ReleaseType" /> matchingAnyText ["Bundle", "Single", "Album", "VideoAlbum", "VideoSingle"])

-- Return all releases with track-level release types ("TrackRelease", "VideoTrackRelease")
trackReleases :: Node -> Nodes
trackReleases = (allTags "ReleaseList" /> tag "Release") `with` (allTags "ReleaseType" /> matchingAnyText ["TrackRelease", "VideoTrackRelease"])

matchingAnyText :: [String] -> Node -> Nodes
matchingAnyText [] = none
matchingAnyText (t:ts) = (matchingText t) |>| (matchingAnyText ts)