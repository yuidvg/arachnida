module MetadataExtractor (extractMetadata) where

import Codec.Picture (DynamicImage (..), dynamicMap, imageHeight, imageWidth, readImageWithMetadata)
import Codec.Picture.Metadata (extractExifMetas)
import Codec.Picture.Metadata.Exif (ExifData (..), ExifTag)
import Data.Char (isAsciiUpper)
import Data.Map qualified as Map
import Data.Maybe (mapMaybe)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import System.Directory (doesFileExist, getFileSize)
import System.EasyFile (getCreationTime)
import System.FilePath (takeExtension, takeFileName)
import Types (FileInfo (..), ImageMetadata (..))

-- | Extract metadata from an image file
extractMetadata :: FilePath -> IO (Maybe ImageMetadata)
extractMetadata filePath = do
  exists <- doesFileExist filePath
  if not exists
    then return Nothing
    else do
      -- Get basic file information
      fileSize <- getFileSize filePath
      creationTime <- getCreationTime filePath
      -- Try to read the image and extract metadata
      result <- readImageWithMetadata filePath
      case result of
        Left _ -> return Nothing -- Failed to read image
        Right (img, metadata) -> do
          let fileInfo =
                FileInfo
                  { name = T.pack $ takeFileName filePath,
                    size = fileSize,
                    format = T.pack $ map toLower $ drop 1 $ takeExtension filePath,
                    dimensions = getDynamicImageDimensions img,
                    creationDateTime = creationTime
                  }

          -- Extract EXIF data
          let exifMetas = extractExifMetas metadata
          let exifData = Map.fromList $ mapMaybe convertExifToText exifMetas

          return $
            Just
              ImageMetadata
                { fileInfo = fileInfo,
                  exifData = exifData
                }

-- | Get dimensions from DynamicImage
getDynamicImageDimensions :: DynamicImage -> (Int, Int)
getDynamicImageDimensions = dynamicMap (\img -> (imageWidth img, imageHeight img))

-- | Convert EXIF metadata to text representation
convertExifToText :: (ExifTag, Codec.Picture.Metadata.Exif.ExifData) -> Maybe (T.Text, T.Text)
convertExifToText (tag, exifVal) = case cleanTagName tag of
  Nothing -> Nothing
  Just tagName -> Just (tagName, cleanExifValue exifVal)

-- | Clean up tag names to be more readable
cleanTagName :: ExifTag -> Maybe T.Text
cleanTagName tag =
  let tagStr = show tag
      tagText = T.pack tagStr
      specialCases = [("TagISOSpeedRatings", "ISO")]
      isUnknownTag = "TagUnknown" `elem` words tagStr
      removeTagPrefix = T.drop 3 -- Remove "Tag" prefix
   in if isUnknownTag
        then Nothing
        else case lookup tagStr specialCases of
          Just special -> Just $ T.pack special
          Nothing ->
            if T.pack "Tag" `T.isPrefixOf` tagText
              then Just $ removeTagPrefix tagText
              else Just tagText

-- | Clean up EXIF values to remove unnecessary prefixes and suffixes
cleanExifValue :: Codec.Picture.Metadata.Exif.ExifData -> T.Text
cleanExifValue exifVal = case exifVal of
  ExifString str -> T.filter (/= '\NUL') $ TE.decodeUtf8 str
  ExifShort val -> T.pack $ show val
  ExifLong val -> T.pack $ show val
  ExifRational num den -> T.pack $ show (fromIntegral num / fromIntegral den :: Double)
  _ -> T.pack $ show exifVal

-- | Convert character to lowercase
toLower :: Char -> Char
toLower c
  | isAsciiUpper c = toEnum (fromEnum c + 32)
  | otherwise = c