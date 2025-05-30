module ExifParser (parseExifData) where

import Control.Monad (replicateM, when)
import Data.Binary.Get
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as LBS
import Data.Text qualified as T
import Data.Word
import Types (ExifData (..))

-- | Parse EXIF data from a ByteString
parseExifData :: BS.ByteString -> Maybe ExifData
parseExifData bs =
  case findExifSegment bs of
    Nothing -> Nothing
    Just exifBytes -> parseExifSegment exifBytes

-- | Find EXIF segment in JPEG file
findExifSegment :: BS.ByteString -> Maybe BS.ByteString
findExifSegment bs
  | BS.length bs < 4 = Nothing
  | BS.take 2 bs /= BS.pack [0xFF, 0xD8] = Nothing -- Not a JPEG
  | otherwise = findExifMarker (BS.drop 2 bs)
  where
    findExifMarker :: BS.ByteString -> Maybe BS.ByteString
    findExifMarker bytes
      | BS.length bytes < 4 = Nothing
      | BS.take 2 bytes == BS.pack [0xFF, 0xE1] -- APP1 marker
        =
          let segmentLength = fromIntegral $ runGet getWord16be (LBS.fromStrict $ BS.drop 2 bytes)
              segmentData = BS.take (segmentLength - 2) (BS.drop 4 bytes)
           in if BS.take 4 segmentData == BS.pack [0x45, 0x78, 0x69, 0x66] -- "Exif"
                then Just (BS.drop 6 segmentData) -- Skip "Exif\0\0"
                else findExifMarker (BS.drop (segmentLength + 2) bytes)
      | BS.head bytes == 0xFF -- Skip other markers
        =
          let markerLength =
                if BS.length bytes >= 4
                  then fromIntegral $ runGet getWord16be (LBS.fromStrict $ BS.drop 2 bytes)
                  else 0
           in findExifMarker (BS.drop (markerLength + 2) bytes)
      | otherwise = Nothing

-- | Parse EXIF segment
parseExifSegment :: BS.ByteString -> Maybe ExifData
parseExifSegment bs
  | BS.length bs < 8 = Nothing
  | otherwise =
      let endianness = BS.take 2 bs
          isBigEndian = endianness == BS.pack [0x4D, 0x4D] -- "MM"
          isLittleEndian = endianness == BS.pack [0x49, 0x49] -- "II"
       in if isBigEndian || isLittleEndian
            then parseIFD isBigEndian bs
            else Nothing

-- | Parse IFD (Image File Directory)
parseIFD :: Bool -> BS.ByteString -> Maybe ExifData
parseIFD isBigEndian bs =
  case runGetOrFail (parseIFDEntries isBigEndian) (LBS.fromStrict bs) of
    Left _ -> Nothing
    Right (_, _, exifData) -> Just exifData

-- | Parse IFD entries
parseIFDEntries :: Bool -> Get ExifData
parseIFDEntries isBigEndian = do
  skip 2 -- Skip byte order marker
  magic <- getWord16 isBigEndian
  when (magic /= 0x002A) $ fail "Invalid TIFF magic number"

  ifdOffset <- getWord32 isBigEndian
  skip (fromIntegral ifdOffset - 8) -- Skip to IFD
  entryCount <- getWord16 isBigEndian
  entries <- replicateM (fromIntegral entryCount) (parseIFDEntry isBigEndian)

  return $ foldl updateExifData emptyExifData entries

-- | Empty EXIF data
emptyExifData :: ExifData
emptyExifData =
  ExifData
    { make = Nothing,
      model = Nothing,
      dateTime = Nothing,
      dateTimeOriginal = Nothing,
      dateTimeDigitized = Nothing,
      orientation = Nothing,
      xResolution = Nothing,
      yResolution = Nothing,
      resolutionUnit = Nothing,
      software = Nothing,
      artist = Nothing,
      copyright = Nothing,
      exposureTime = Nothing,
      fNumber = Nothing,
      iso = Nothing,
      focalLength = Nothing,
      flash = Nothing,
      whiteBalance = Nothing,
      gpsLatitude = Nothing,
      gpsLongitude = Nothing,
      gpsAltitude = Nothing
    }

-- | IFD Entry
data IFDEntry = IFDEntry
  { tag :: Word16,
    dataType :: Word16,
    count :: Word32,
    value :: Word32
  }
  deriving (Show)

-- | Parse single IFD entry
parseIFDEntry :: Bool -> Get IFDEntry
parseIFDEntry isBigEndian =
  IFDEntry
    <$> getWord16 isBigEndian
    <*> getWord16 isBigEndian
    <*> getWord32 isBigEndian
    <*> getWord32 isBigEndian

-- | Update EXIF data with IFD entry
updateExifData :: ExifData -> IFDEntry -> ExifData
updateExifData exif entry =
  case entry.tag of
    0x010F -> exif {make = Just $ T.pack "Camera Make"} -- Simplified for now
    0x0110 -> exif {model = Just $ T.pack "Camera Model"}
    0x0112 -> exif {orientation = Just $ fromIntegral entry.value}
    _ -> exif

-- | Get Word16 with endianness
getWord16 :: Bool -> Get Word16
getWord16 True = getWord16be
getWord16 False = getWord16le

-- | Get Word32 with endianness
getWord32 :: Bool -> Get Word32
getWord32 True = getWord32be
getWord32 False = getWord32le