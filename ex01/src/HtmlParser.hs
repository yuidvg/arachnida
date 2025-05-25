module HtmlParser
  ( parseHtml,
    extractLinks,
    extractImages,
    resolveUrl,
  )
where

import Data.List (isSuffixOf)
import Data.Maybe (fromMaybe, isJust, mapMaybe)
import Network.URI
import Text.HTML.TagSoup

-- | Parse HTML string into tags
parseHtml :: String -> [Tag String]
parseHtml = parseTags

-- | Extract all links (<a href>) from HTML tags
extractLinks :: String -> [Tag String] -> [String]
extractLinks baseUrl = mapMaybe getHref
  where
    getHref :: Tag String -> Maybe String
    getHref tag = case tag of
      (TagOpen "a" attrs) -> resolveUrl baseUrl <$> lookup "href" attrs
      _ -> Nothing

-- | Extract all image URLs (<img src>) from HTML tags
extractImages :: String -> [String] -> [Tag String] -> [String]
extractImages baseUrl extensions = mapMaybe (getImageUrl baseUrl extensions)

-- | Extract image URL if it has one of the specified extensions
getImageUrl :: String -> [String] -> Tag String -> Maybe String
getImageUrl baseUrl extensions tag = case tag of
  (TagOpen "img" attrs) -> do
    src <- lookup "src" attrs
    let resolvedUrl = resolveUrl baseUrl src
    if any (`isSuffixOf` resolvedUrl) extensions
      then Just resolvedUrl
      else Nothing
  _ -> Nothing

-- | Resolve a relative URL against a base URL
resolveUrl :: String -> String -> String
resolveUrl baseUrl relUrl =
  case parseURIReference relUrl of
    Nothing -> relUrl -- Can't parse relative URL, return as is
    Just relUri ->
      case parseURI baseUrl of
        Nothing -> relUrl -- Can't parse base URL, return as is
        Just baseUri ->
          show (nonStrictRelativeTo relUri baseUri) -- nonStrictRelativeTo returns a URI directly