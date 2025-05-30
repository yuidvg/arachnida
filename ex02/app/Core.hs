module Core (processFiles, formatMetadata) where

import Control.Monad (forM_)
import Data.Char (isAsciiUpper)
import Data.Map qualified as Map
import Data.Text qualified as T
import MetadataExtractor (extractMetadata)
import System.FilePath (takeExtension)
import Types (Args (..), ExifData (..), FileMetadata (..), ImageMetadata (..), extensions)

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
      "  Name: " ++ T.unpack metadata.fileMetadata.fileName,
      "  Size: " ++ formatFileSize metadata.fileMetadata.fileSize,
      "  Type: " ++ T.unpack metadata.fileMetadata.fileType
    ]
      ++ formatDimensions metadata.fileMetadata.dimensions
      ++ formatExifData metadata.exifData
      ++ formatRawMetadata metadata.rawMetadata

-- | Format file size in human-readable format
formatFileSize :: Integer -> String
formatFileSize size
  | size < 1024 = show size ++ " bytes"
  | size < 1024 * 1024 = show (size `div` 1024) ++ " KB"
  | size < 1024 * 1024 * 1024 = show (size `div` (1024 * 1024)) ++ " MB"
  | otherwise = show (size `div` (1024 * 1024 * 1024)) ++ " GB"

-- | Format image dimensions
formatDimensions :: Maybe (Int, Int) -> [String]
formatDimensions Nothing = []
formatDimensions (Just (width, height)) =
  ["  Dimensions: " ++ show width ++ " x " ++ show height ++ " pixels"]

-- | Format EXIF data
formatExifData :: Maybe ExifData -> [String]
formatExifData Nothing = ["", "EXIF Data: Not available"]
formatExifData (Just exif) =
  ["", "EXIF Data:"]
    ++ concatMap
      formatExifField
      [ ("Make", fmap T.unpack exif.make),
        ("Model", fmap T.unpack exif.model),
        ("Software", fmap T.unpack exif.software),
        ("Host Computer", fmap T.unpack exif.hostComputer),
        ("Artist", fmap T.unpack exif.artist),
        ("Copyright", fmap T.unpack exif.copyright),
        ("Orientation", fmap show exif.orientation),
        ("X Resolution", fmap show exif.xResolution),
        ("Y Resolution", fmap show exif.yResolution),
        ("Resolution Unit", fmap show exif.resolutionUnit),
        ("Exposure Time", fmap show exif.exposureTime),
        ("F Number", fmap show exif.fNumber),
        ("ISO", fmap show exif.iso),
        ("Focal Length", fmap show exif.focalLength),
        ("Focal Length (35mm)", fmap show exif.focalLengthIn35mm),
        ("Flash", fmap show exif.flash),
        ("White Balance", fmap show exif.whiteBalance),
        ("Exposure Program", fmap T.unpack exif.exposureProgram),
        ("Metering Mode", fmap T.unpack exif.meteringMode),
        ("Scene Type", fmap T.unpack exif.sceneType),
        ("Exposure Mode", fmap T.unpack exif.exposureMode),
        ("Scene Capture Type", fmap T.unpack exif.sceneCaptureType),
        ("Color Space", fmap T.unpack exif.colorSpace),
        ("Sensing Method", fmap T.unpack exif.sensingMethod),
        ("EXIF Image Width", fmap show exif.exifImageWidth),
        ("EXIF Image Height", fmap show exif.exifImageHeight),
        ("Lens Info", fmap T.unpack exif.lensInfo),
        ("Lens Make", fmap T.unpack exif.lensMake),
        ("Lens Model", fmap T.unpack exif.lensModel),
        ("GPS Latitude", fmap show exif.gpsLatitude),
        ("GPS Longitude", fmap show exif.gpsLongitude),
        ("GPS Altitude", fmap show exif.gpsAltitude)
      ]

-- | Format individual EXIF field
formatExifField :: (String, Maybe String) -> [String]
formatExifField (_, Nothing) = []
formatExifField (name, Just value) = ["  " ++ name ++ ": " ++ value]

-- | Format raw metadata
formatRawMetadata :: Map.Map T.Text T.Text -> [String]
formatRawMetadata rawData
  | Map.null rawData = []
  | otherwise =
      ["", "Raw Metadata:"]
        ++ map formatRawField (Map.toList rawData)

-- | Format individual raw metadata field
formatRawField :: (T.Text, T.Text) -> String
formatRawField (key, value) = "  " ++ T.unpack key ++ ": " ++ T.unpack value

-- | Convert character to lowercase
toLower :: Char -> Char
toLower c
  | isAsciiUpper c = toEnum (fromEnum c + 32)
  | otherwise = c