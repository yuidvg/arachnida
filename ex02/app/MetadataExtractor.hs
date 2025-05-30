module MetadataExtractor (extractMetadata) where

import Data.ByteString qualified as BS
import Data.Map qualified as Map
import Data.Text qualified as T
import ExifParser (parseExifData)
import System.Directory (doesFileExist, getFileSize)
import System.FilePath (takeExtension, takeFileName)
import Types (FileMetadata (..), ImageMetadata (..))

-- | Extract metadata from an image file
extractMetadata :: FilePath -> IO (Maybe ImageMetadata)
extractMetadata filePath = do
  exists <- doesFileExist filePath
  if not exists
    then return Nothing
    else do
      fileSize <- getFileSize filePath
      fileContent <- BS.readFile filePath

      let fileMetadata =
            FileMetadata
              { fileName = T.pack $ takeFileName filePath,
                fileSize = fileSize,
                fileType = T.pack $ map toLower $ drop 1 $ takeExtension filePath,
                dimensions = extractDimensions fileContent,
                colorDepth = Nothing,
                compression = Nothing
              }

      let exifData = parseExifData fileContent
      let rawMetadata = extractRawMetadata fileContent

      return $
        Just
          ImageMetadata
            { fileMetadata = fileMetadata,
              exifData = exifData,
              rawMetadata = rawMetadata
            }

-- | Extract image dimensions from file content
extractDimensions :: BS.ByteString -> Maybe (Int, Int)
extractDimensions bs
  | isJPEG bs = extractJPEGDimensions bs
  | isPNG bs = extractPNGDimensions bs
  | isGIF bs = extractGIFDimensions bs
  | isBMP bs = extractBMPDimensions bs
  | otherwise = Nothing

-- | Check if file is JPEG
isJPEG :: BS.ByteString -> Bool
isJPEG bs = BS.length bs >= 2 && BS.take 2 bs == BS.pack [0xFF, 0xD8]

-- | Check if file is PNG
isPNG :: BS.ByteString -> Bool
isPNG bs = BS.length bs >= 8 && BS.take 8 bs == BS.pack [0x89, 0x50, 0x4E, 0x47, 0x0D, 0x0A, 0x1A, 0x0A]

-- | Check if file is GIF
isGIF :: BS.ByteString -> Bool
isGIF bs =
  BS.length bs >= 6
    && ( BS.take 6 bs == BS.pack [0x47, 0x49, 0x46, 0x38, 0x37, 0x61]
           || BS.take 6 bs == BS.pack [0x47, 0x49, 0x46, 0x38, 0x39, 0x61] -- GIF87a
           -- GIF89a
       )

-- | Check if file is BMP
isBMP :: BS.ByteString -> Bool
isBMP bs = BS.length bs >= 2 && BS.take 2 bs == BS.pack [0x42, 0x4D] -- "BM"

-- | Extract JPEG dimensions
extractJPEGDimensions :: BS.ByteString -> Maybe (Int, Int)
extractJPEGDimensions bs = findSOFMarker (BS.drop 2 bs)
  where
    findSOFMarker :: BS.ByteString -> Maybe (Int, Int)
    findSOFMarker bytes
      | BS.length bytes < 4 = Nothing
      | BS.take 2 bytes == BS.pack [0xFF, 0xC0] -- SOF0 marker
        =
          if BS.length bytes >= 9
            then
              let height = fromIntegral (BS.index bytes 5) * 256 + fromIntegral (BS.index bytes 6)
                  width = fromIntegral (BS.index bytes 7) * 256 + fromIntegral (BS.index bytes 8)
               in Just (width, height)
            else Nothing
      | BS.head bytes == 0xFF -- Skip other markers
        =
          let markerLength =
                if BS.length bytes >= 4
                  then fromIntegral (BS.index bytes 2) * 256 + fromIntegral (BS.index bytes 3)
                  else 0
           in findSOFMarker (BS.drop (markerLength + 2) bytes)
      | otherwise = Nothing

-- | Extract PNG dimensions
extractPNGDimensions :: BS.ByteString -> Maybe (Int, Int)
extractPNGDimensions bs
  | BS.length bs >= 24 =
      let width =
            fromIntegral (BS.index bs 16) * 16777216
              + fromIntegral (BS.index bs 17) * 65536
              + fromIntegral (BS.index bs 18) * 256
              + fromIntegral (BS.index bs 19)
          height =
            fromIntegral (BS.index bs 20) * 16777216
              + fromIntegral (BS.index bs 21) * 65536
              + fromIntegral (BS.index bs 22) * 256
              + fromIntegral (BS.index bs 23)
       in Just (width, height)
  | otherwise = Nothing

-- | Extract GIF dimensions
extractGIFDimensions :: BS.ByteString -> Maybe (Int, Int)
extractGIFDimensions bs
  | BS.length bs >= 10 =
      let width = fromIntegral (BS.index bs 7) * 256 + fromIntegral (BS.index bs 6)
          height = fromIntegral (BS.index bs 9) * 256 + fromIntegral (BS.index bs 8)
       in Just (width, height)
  | otherwise = Nothing

-- | Extract BMP dimensions
extractBMPDimensions :: BS.ByteString -> Maybe (Int, Int)
extractBMPDimensions bs
  | BS.length bs >= 26 =
      let width =
            fromIntegral (BS.index bs 21) * 16777216
              + fromIntegral (BS.index bs 20) * 65536
              + fromIntegral (BS.index bs 19) * 256
              + fromIntegral (BS.index bs 18)
          height =
            fromIntegral (BS.index bs 25) * 16777216
              + fromIntegral (BS.index bs 24) * 65536
              + fromIntegral (BS.index bs 23) * 256
              + fromIntegral (BS.index bs 22)
       in Just (width, height)
  | otherwise = Nothing

-- | Extract raw metadata as key-value pairs
extractRawMetadata :: BS.ByteString -> Map.Map T.Text T.Text
extractRawMetadata bs =
  Map.fromList
    [ (T.pack "file_signature", T.pack $ show $ BS.take 8 bs),
      (T.pack "file_size", T.pack $ show $ BS.length bs)
    ]

-- | Convert character to lowercase
toLower :: Char -> Char
toLower c
  | c >= 'A' && c <= 'Z' = toEnum (fromEnum c + 32)
  | otherwise = c