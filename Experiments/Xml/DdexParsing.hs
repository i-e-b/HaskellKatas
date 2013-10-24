
-- DDEX specific parsing functions.
-- Use this to decompose an XML message into a working record set.
module DdexParsing
	( allDeals, senderId, releaseTitle, productRelease, releaseReference, trackReleases, releasePrimaryResources, resourceById
	, trackISRC) where

import Text.XML.HaXml.Combinators
import HaXmlHelper

-- All deals for all releases
allDeals :: Filter
allDeals = allTags "Deal"

-- Sender ID, preferring "SentOnBehalfOf", returning "MessageSender" otherwise
senderId :: Filter
senderId = (allTags "MessageHeader" /> tag "SentOnBehalfOf" /> tag "PartyId")
	|>| (allTags "MessageHeader" /> tag "MessageSender" /> tag "PartyId") -- `|>|` means output right only if no left.

-- Title of a release
releaseTitle :: Filter
releaseTitle = allTags "ReferenceTitle" /> tag "TitleText"

-- Reference codes for a release
releaseReference :: Node -> String
releaseReference n = innerText (allTags "ReleaseReference" n)

-- All primary resources for a release
releasePrimaryResources :: Node -> [String]
releasePrimaryResources n = 
	let filter = (allTags "ReleaseResourceReferenceList" /> tag "ReleaseResourceReference") `with` (exactAttribute "ReleaseResourceType" "PrimaryResource")
	in  innerTexts (filter n)

-- get the isrc tag for a sound recording
trackISRC :: Node -> String
trackISRC n = innerText (allTags "SoundRecording" /> tag "SoundRecordingId" /> tag "ISRC" $ n)

-- pick a sound recording by resource ID.
-- this should try to generalise by returning any kind of resource.
resourceById :: String -> Filter
resourceById id = (allTags "ResourceList" /> tag "SoundRecording") `with` (allTags "ResourceReference" /> matchingText id)

-- Return all releases with Product-level release types ("Bundle", "Single", "Album", "VideoAlbum", "VideoSingle")
productRelease :: Filter
productRelease = (allTags "ReleaseList" /> tag "Release") `with` (allTags "ReleaseType" /> matchingAnyText ["Bundle", "Single", "Album", "VideoAlbum", "VideoSingle"])

-- Return all releases with track-level release types ("TrackRelease", "VideoTrackRelease")
trackReleases :: Filter
trackReleases = (allTags "ReleaseList" /> tag "Release") `with` (allTags "ReleaseType" /> matchingAnyText ["TrackRelease", "VideoTrackRelease"])
