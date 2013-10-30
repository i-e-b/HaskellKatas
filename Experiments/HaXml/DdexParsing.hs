
-- DDEX specific parsing functions.
-- Use this to decompose an XML message into a working record set.
module DdexParsing
	( allDeals, senderId, releaseTitle, productRelease, releaseReference, trackReleases, releasePrimaryResources, resourceById
	, trackISRC, dealsForRelease, downloadTerritories) where

import Text.XML.HaXml.Combinators
import HaXmlHelper

-- Deal groups for a given release ID
dealsForRelease :: String -> Filter
dealsForRelease releaseId = ((allTags "ReleaseDeal") `with` (allTags "DealReleaseReference" /> matchingText releaseId)) /> tag "Deal"

-- PAYG/Permanent download territories expressed in a deal node
downloadTerritories :: Nodes -> [String]
downloadTerritories dealNode = 
	let payg = (allTags "CommercialModelType" /> matchingText "PayAsYouGoModel")
	    permDown = (allTags "Usage" /> tag "UseType" /> matchingText "PermanentDownload")
	    territories = (allTags "DealTerms" `with` payg `with` permDown) /> tag "TerritoryCode"
	in  texts (territories) dealNode

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

-- given a resource id and release id, choose the best available display title.
-- Resource
--      <Title TitleType="DisplayTitle">
-- Release, details by territory:
--      <Title TitleType="DisplayTitle">
--          <TitleText>Take the 'A' Train</TitleText>
-- Release:
--      <ReferenceTitle>
--          <TitleText>Take the 'A' Train</TitleText>
--

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
