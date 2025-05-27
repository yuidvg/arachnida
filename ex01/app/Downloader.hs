module Downloader
  ( downloadImage,
  )
where

import Control.Exception (SomeException, catch)
import Data.ByteString.Lazy as LBS
import Network.HTTP.Conduit (simpleHttp)
import System.FilePath (takeFileName, (</>))
import System.IO (hPutStrLn, stderr)

-- | Download an image from a URL and save it to the specified directory
downloadImage :: String -> FilePath -> IO ()
downloadImage url savePath = do
  result <- catch (downloadImage' url savePath) handleException
  case result of
    Left err -> hPutStrLn stderr $ "Failed to download " ++ url ++ ": " ++ err
    Right _ -> return ()
  where
    handleException :: SomeException -> IO (Either String ())
    handleException e = return $ Left $ show e

-- | Internal function to perform the actual download
downloadImage' :: String -> FilePath -> IO (Either String ())
downloadImage' url savePath = do
  imageData <- simpleHttp url
  let fileName = takeFileName url
      filePath = savePath </> fileName

  LBS.writeFile filePath imageData
  putStrLn $ "  Saved: " ++ filePath
  return $ Right ()