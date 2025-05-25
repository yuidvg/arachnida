module Downloader
  ( fetchHtml,
    downloadImage,
    prepareDownloadDir,
  )
where

import Control.Exception (SomeException, try)
import Data.ByteString.Char8 qualified as BS
import Data.ByteString.Lazy qualified as LBS
import Network.HTTP.Simple
import Network.URI (parseURI)
import System.Directory (createDirectoryIfMissing)
import System.FilePath (takeFileName, (</>))

-- | Fetch HTML content from a URL
fetchHtml :: String -> IO (Either String String)
fetchHtml url = do
  result <- try $ do
    request <- parseRequest url
    response <- httpLBS request
    let status = getResponseStatusCode response
    if status == 200
      then pure $ Right $ BS.unpack $ LBS.toStrict $ getResponseBody response
      else pure $ Left $ "Failed to fetch URL, status code: " ++ show status
  case result of
    Left e -> pure $ Left $ "Error fetching URL: " ++ show (e :: SomeException)
    Right r -> pure r

-- | Download an image from a URL and save it to a file
downloadImage :: FilePath -> String -> IO (Either String FilePath)
downloadImage basePath url = do
  result <- try $ do
    request <- parseRequest url
    response <- httpLBS request
    let status = getResponseStatusCode response
    if status == 200
      then do
        let filename = takeFileName url
        let filepath = basePath </> sanitizeFilename filename
        LBS.writeFile filepath (getResponseBody response)
        pure $ Right filepath
      else pure $ Left $ "Failed to download image, status code: " ++ show status
  case result of
    Left e -> pure $ Left $ "Error downloading image: " ++ show (e :: SomeException)
    Right r -> pure r

-- | Create the download directory if it doesn't exist
prepareDownloadDir :: FilePath -> IO ()
prepareDownloadDir = createDirectoryIfMissing True

-- | Sanitize a filename by replacing invalid characters
sanitizeFilename :: String -> String
sanitizeFilename = map replaceInvalidChar
  where
    replaceInvalidChar c
      | c `elem` invalidChars = '_'
      | otherwise = c
    invalidChars = "/\\?%*:|\"<>"