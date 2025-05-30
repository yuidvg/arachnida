module Types where

import Data.Map qualified as Map
import Data.Text (Text)
import Data.Time (UTCTime)

-- | Represents EXIF data extracted from an image
data ExifData = ExifData
  { make :: Maybe Text,
    model :: Maybe Text,
    dateTime :: Maybe UTCTime,
    dateTimeOriginal :: Maybe UTCTime,
    dateTimeDigitized :: Maybe UTCTime,
    orientation :: Maybe Int,
    xResolution :: Maybe Double,
    yResolution :: Maybe Double,
    resolutionUnit :: Maybe Int,
    software :: Maybe Text,
    artist :: Maybe Text,
    copyright :: Maybe Text,
    exposureTime :: Maybe Double,
    fNumber :: Maybe Double,
    iso :: Maybe Int,
    focalLength :: Maybe Double,
    flash :: Maybe Int,
    whiteBalance :: Maybe Int,
    gpsLatitude :: Maybe Double,
    gpsLongitude :: Maybe Double,
    gpsAltitude :: Maybe Double
  }
  deriving (Show, Eq)

-- | Represents basic file metadata
data FileMetadata = FileMetadata
  { fileName :: Text,
    fileSize :: Integer,
    fileType :: Text,
    dimensions :: Maybe (Int, Int),
    colorDepth :: Maybe Int,
    compression :: Maybe Text
  }
  deriving (Show, Eq)

-- | Combined metadata for an image file
data ImageMetadata = ImageMetadata
  { fileMetadata :: FileMetadata,
    exifData :: Maybe ExifData,
    rawMetadata :: Map.Map Text Text
  }
  deriving (Show, Eq)

-- | Command line arguments
data Args = Args
  { inputFiles :: [FilePath]
  }
  deriving (Show, Eq)