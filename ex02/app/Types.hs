module Types
  ( Args (..),
    extensions,
    FileInfo (..),
    ExifData,
    ImageMetadata (..),
  )
where

import Data.Map qualified as Map
import Data.Text (Text)
import Data.Time (UTCTime)

-- | Command line arguments
newtype Args = Args
  { inputFiles :: [FilePath]
  }
  deriving (Show, Eq)

-- | Supported image file extensions
extensions :: [String]
extensions = [".jpg", ".jpeg", ".png", ".gif", ".bmp"]

-- | Basic file information
data FileInfo = FileInfo
  { fileName :: Text,
    fileSize :: Integer,
    fileFormat :: Text,
    dimensions :: Maybe (Int, Int), -- (width, height)
    modificationDateTime :: Maybe UTCTime
  }
  deriving (Show, Eq)

-- | EXIF data as key-value pairs
type ExifData = Map.Map Text Text

-- | Complete image metadata
data ImageMetadata = ImageMetadata
  { fileInfo :: FileInfo,
    exifData :: ExifData,
    rawMetadata :: Map.Map Text Text -- Additional metadata
  }
  deriving (Show, Eq)
