module HtmlParser
  ( extractImageUrls,
    extractLinks,
  )
where

import Control.Exception (SomeException, catch)
import Data.ByteString.Lazy.Char8 qualified as L8
import Data.Char (isAsciiUpper)
import Data.List (isSuffixOf)
import Network.HTTP.Conduit (simpleHttp)
import Network.URI (parseURI, parseURIReference, relativeTo)
import System.IO (hPutStrLn, stderr)
import Text.HTML.TagSoup (Tag (..), fromAttrib, parseTags)

-- | Extract image URLs from a webpage that match the given extensions
extractImageUrls :: [String] -> String -> IO [String]
extractImageUrls extensions url = do
  result <- catch (extractImageUrls' extensions url) handleException
  case result of
    Left err -> do
      hPutStrLn stderr $ "Failed to extract images from " ++ url ++ ": " ++ err
      return []
    Right urls -> return urls
  where
    handleException :: SomeException -> IO (Either String [String])
    handleException e = return $ Left $ show e

-- | Internal function to extract image URLs
extractImageUrls' :: [String] -> String -> IO (Either String [String])
extractImageUrls' extensions url = do
  html <- simpleHttp url
  let tags = parseTags $ L8.unpack html
      imgTags = [tag | tag@(TagOpen "img" _) <- tags]
      imageUrls = [fromAttrib "src" tag | tag <- imgTags]
      filteredUrls = filter (hasValidExtension extensions) imageUrls
      absoluteUrls = map (makeAbsolute url) filteredUrls
  return $ Right $ filter (not . null) absoluteUrls

-- | Extract all links from a webpage for recursive crawling
extractLinks :: String -> IO [String]
extractLinks url = do
  result <- catch (extractLinks' url) handleException
  case result of
    Left err -> do
      hPutStrLn stderr $ "Failed to extract links from " ++ url ++ ": " ++ err
      return []
    Right urls -> return urls
  where
    handleException :: SomeException -> IO (Either String [String])
    handleException e = return $ Left $ show e

-- | Internal function to extract links
extractLinks' :: String -> IO (Either String [String])
extractLinks' url = do
  html <- simpleHttp url
  let tags = parseTags $ L8.unpack html
      linkTags = [tag | tag@(TagOpen "a" _) <- tags]
      linkUrls = [fromAttrib "href" tag | tag <- linkTags]
      absoluteUrls = map (makeAbsolute url) linkUrls
  return $ Right $ filter (not . null) absoluteUrls

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