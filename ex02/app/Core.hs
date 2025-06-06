module Core (processFiles, formatMetadata) where

import Control.Monad (forM_)
import Data.Char (isAsciiUpper)
import Data.Map qualified as Map
import Data.Text qualified as T
import Data.Time (UTCTime)
import MetadataExtractor (extractMetadata)
import System.FilePath (takeExtension)
import Types (Args (..), ExifData, FileInfo (..), ImageMetadata (..), extensions)

-- | Process multiple image files
processFiles :: Args -> IO ()
processFiles args = do
  forM_ args.inputFiles $ \filePath -> do
    if isValidImageFile filePath
      then do
        putStrLn $ "Processing: " ++ filePath
        putStrLn $ replicate (length filePath + 12) '-'

        maybeMetadata <- extractMetadata filePath
        case maybeMetadata of
          Nothing -> putStrLn "Error: Could not extract metadata"
          Just metadata -> do
            putStrLn $ formatMetadata metadata
            putStrLn ""
      else do
        putStrLn $ "Skipping unsupported file: " ++ filePath

-- | Check if file has a valid image extension
isValidImageFile :: FilePath -> Bool
isValidImageFile path =
  let ext = map toLower $ takeExtension path
   in ext `elem` extensions

-- | Format metadata for display
formatMetadata :: ImageMetadata -> String
formatMetadata metadata =
  unlines $
    [ "File Information:",
      "  Name: " ++ T.unpack metadata.fileInfo.name,
      "  Size: " ++ formatFileSize metadata.fileInfo.size,
      "  Format: " ++ T.unpack metadata.fileInfo.format
    ]
      ++ formatDimensions metadata.fileInfo.dimensions
      ++ formatCreationDate metadata.fileInfo.creationDateTime
      ++ formatExifData metadata.exifData

-- | Format file size in human-readable format
formatFileSize :: Integer -> String
formatFileSize size
  | size < 1024 = show size ++ " bytes"
  | size < 1024 * 1024 = show (size `div` 1024) ++ " KB"
  | size < 1024 * 1024 * 1024 = show (size `div` (1024 * 1024)) ++ " MB"
  | otherwise = show (size `div` (1024 * 1024 * 1024)) ++ " GB"

-- | Format image dimensions
formatDimensions :: (Int, Int) -> [String]
formatDimensions (width, height) =
  ["  Dimensions: " ++ show width ++ " x " ++ show height ++ " pixels"]

-- | Format creation date
formatCreationDate :: Maybe UTCTime -> [String]
formatCreationDate Nothing = ["  Creation Date: Not available"]
formatCreationDate (Just time) = ["  Creation Date: " ++ show time]

-- | Format EXIF data
formatExifData :: ExifData -> [String]
formatExifData exifMap
  | Map.null exifMap = ["", "EXIF Data: Not available"]
  | otherwise =
      ["", "EXIF Data:"]
        ++ map formatExifField (Map.toList exifMap)

-- | Format individual EXIF field
formatExifField :: (T.Text, T.Text) -> String
formatExifField (name, value) = "  " ++ T.unpack name ++ ": " ++ T.unpack value

-- | Convert character to lowercase
toLower :: Char -> Char
toLower c
  | isAsciiUpper c = toEnum (fromEnum c + 32)
  | otherwise = c