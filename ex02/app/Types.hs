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
  { name :: Text,
    size :: Integer,
    format :: Text,
    dimensions :: (Int, Int),
    creationDateTime :: Maybe UTCTime
  }
  deriving (Show, Eq)

-- | EXIF data as key-value pairs
type ExifData = Map.Map Text Text

-- | Complete image metadata
data ImageMetadata = ImageMetadata
  { fileInfo :: FileInfo,
    exifData :: ExifData
  }
  deriving (Show, Eq)
