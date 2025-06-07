module HtmlParser
  ( extractImageUrls,
    extractLinks,
  )
where

import Data.ByteString.Lazy.Char8 qualified as L8
import Data.Char (isAsciiUpper)
import Data.List (isSuffixOf)
import Network.URI (parseURI, parseURIReference, relativeTo)
import Text.HTML.TagSoup (Tag (..), fromAttrib, parseTags)

-- | Extract image URLs from a webpage that match the given extensions
extractImageUrls :: [String] -> String -> L8.ByteString -> [String]
extractImageUrls extensions baseUrl html =
  filter (not . null) $ map (makeAbsolute baseUrl) filteredUrls
  where
    filteredUrls = filter (hasValidExtension extensions) imageUrls
    imageUrls = [fromAttrib "src" tag | tag <- imgTags]
    imgTags = [tag | tag@(TagOpen "img" _) <- tags]
    tags = parseTags $ L8.unpack html

-- | Extract all links from a webpage for recursive crawling
extractLinks :: String -> L8.ByteString -> [String]
extractLinks baseUrl html =
  filter (not . null) $ map (makeAbsolute baseUrl) linkUrls
  where
    linkUrls = [fromAttrib "href" tag | tag <- linkTags]
    linkTags = [tag | tag@(TagOpen "a" _) <- tags]
    tags = parseTags $ L8.unpack html

-- | Check if a URL has a valid image extension
hasValidExtension :: [String] -> String -> Bool
hasValidExtension extensions urlStr =
  any ((`isSuffixOf` map toLower urlStr) . map toLower) extensions
  where
    toLower c
      | isAsciiUpper c = toEnum (fromEnum c + 32)
      | otherwise = c

-- | Convert relative URLs to absolute URLs using Network.URI library
makeAbsolute :: String -> String -> String
makeAbsolute baseUrl relativeUrl
  | null relativeUrl = ""
  | otherwise =
      case parseURI baseUrl of
        Nothing -> ""
        Just base ->
          case parseURIReference relativeUrl of
            Nothing -> ""
            Just relative -> show $ relative `relativeTo` base