{-# LANGUAGE MonoLocalBinds #-}

module MetadataExtractor (extractMetadata) where

import Codec.Picture (DynamicImage (..), imageHeight, imageWidth, readImageWithMetadata)
import Codec.Picture.Metadata (Elem (..), Keys (..), Metadatas, extractExifMetas)
import Codec.Picture.Metadata qualified as Meta
import Codec.Picture.Metadata.Exif (ExifData (..), ExifTag)
import Data.Char (isAsciiUpper)
import Data.Map qualified as Map
import Data.Maybe (mapMaybe)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import System.Directory (doesFileExist, getFileSize, getModificationTime)
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
      modTime <- getModificationTime filePath

      -- Try to read the image and extract metadata
      result <- readImageWithMetadata filePath
      case result of
        Left _ -> return Nothing -- Failed to read image
        Right (img, metadata) -> do
          let fileInfo =
                FileInfo
                  { fileName = T.pack $ takeFileName filePath,
                    fileSize = fileSize,
                    fileFormat = T.pack $ map toLower $ drop 1 $ takeExtension filePath,
                    dimensions = Just (getDynamicImageDimensions img),
                    creationDate = Just modTime
                  }

          -- Extract EXIF data
          let exifMetas = extractExifMetas metadata
          let exifData = Map.fromList $ mapMaybe convertExifToText exifMetas

          -- Extract other metadata
          let rawMeta = extractOtherMetadata metadata

          return $
            Just
              ImageMetadata
                { fileInfo = fileInfo,
                  exifData = exifData,
                  rawMetadata = rawMeta
                }

-- | Get dimensions from DynamicImage
getDynamicImageDimensions :: DynamicImage -> (Int, Int)
getDynamicImageDimensions dynImg = case dynImg of
  ImageY8 img -> (imageWidth img, imageHeight img)
  ImageY16 img -> (imageWidth img, imageHeight img)
  ImageY32 img -> (imageWidth img, imageHeight img)
  ImageYF img -> (imageWidth img, imageHeight img)
  ImageYA8 img -> (imageWidth img, imageHeight img)
  ImageYA16 img -> (imageWidth img, imageHeight img)
  ImageRGB8 img -> (imageWidth img, imageHeight img)
  ImageRGB16 img -> (imageWidth img, imageHeight img)
  ImageRGBF img -> (imageWidth img, imageHeight img)
  ImageRGBA8 img -> (imageWidth img, imageHeight img)
  ImageRGBA16 img -> (imageWidth img, imageHeight img)
  ImageYCbCr8 img -> (imageWidth img, imageHeight img)
  ImageCMYK8 img -> (imageWidth img, imageHeight img)
  ImageCMYK16 img -> (imageWidth img, imageHeight img)

-- | Convert EXIF metadata to text representation
convertExifToText :: (ExifTag, Codec.Picture.Metadata.Exif.ExifData) -> Maybe (T.Text, T.Text)
convertExifToText (tag, exifVal) = case cleanTagName tag of
  Nothing -> Nothing
  Just tagName -> Just (tagName, cleanExifValue exifVal)

-- | Clean up tag names to be more readable
cleanTagName :: ExifTag -> Maybe T.Text
cleanTagName tag = case show tag of
  "TagMake" -> Just $ T.pack "Make"
  "TagModel" -> Just $ T.pack "Model"
  "TagOrientation" -> Just $ T.pack "Orientation"
  "TagFlash" -> Just $ T.pack "Flash"
  "TagDateTime" -> Just $ T.pack "DateTime"
  "TagDateTimeOriginal" -> Just $ T.pack "DateTimeOriginal"
  "TagDateTimeDigitized" -> Just $ T.pack "DateTimeDigitized"
  "TagExposureTime" -> Just $ T.pack "ExposureTime"
  "TagFNumber" -> Just $ T.pack "FNumber"
  "TagISOSpeedRatings" -> Just $ T.pack "ISO"
  "TagFocalLength" -> Just $ T.pack "FocalLength"
  "TagWhiteBalance" -> Just $ T.pack "WhiteBalance"
  "TagExposureProgram" -> Just $ T.pack "ExposureProgram"
  "TagMeteringMode" -> Just $ T.pack "MeteringMode"
  "TagLightSource" -> Just $ T.pack "LightSource"
  "TagColorSpace" -> Just $ T.pack "ColorSpace"
  "TagPixelXDimension" -> Just $ T.pack "PixelXDimension"
  "TagPixelYDimension" -> Just $ T.pack "PixelYDimension"
  tagStr -> if "TagUnknown" `elem` words tagStr then Nothing else Just (T.pack tagStr)

-- | Clean up EXIF values to remove unnecessary prefixes and suffixes
cleanExifValue :: Codec.Picture.Metadata.Exif.ExifData -> T.Text
cleanExifValue exifVal = case exifVal of
  ExifString str -> T.filter (/= '\NUL') $ TE.decodeUtf8 str
  ExifShort val -> T.pack $ show val
  ExifLong val -> T.pack $ show val
  ExifRational num den -> T.pack $ show (fromIntegral num / fromIntegral den :: Double)
  _ -> T.pack $ show exifVal

-- | Extract other metadata from JuicyPixels metadata
extractOtherMetadata :: Metadatas -> Map.Map T.Text T.Text
extractOtherMetadata = Meta.foldMap extractKeyValue
  where
    extractKeyValue :: Elem Keys -> Map.Map T.Text T.Text
    extractKeyValue (key :=> value) = case key of
      DpiX -> Map.singleton (T.pack "DpiX") (T.pack $ show value)
      DpiY -> Map.singleton (T.pack "DpiY") (T.pack $ show value)
      Width -> Map.singleton (T.pack "Width") (T.pack $ show value)
      Height -> Map.singleton (T.pack "Height") (T.pack $ show value)
      Title -> Map.singleton (T.pack "Title") (T.pack value)
      Description -> Map.singleton (T.pack "Description") (T.pack value)
      Author -> Map.singleton (T.pack "Author") (T.pack value)
      Copyright -> Map.singleton (T.pack "Copyright") (T.pack value)
      Software -> Map.singleton (T.pack "Software") (T.pack value)
      Comment -> Map.singleton (T.pack "Comment") (T.pack value)
      _ -> Map.empty

-- | Convert character to lowercase
toLower :: Char -> Char
toLower c
  | isAsciiUpper c = toEnum (fromEnum c + 32)
  | otherwise = c