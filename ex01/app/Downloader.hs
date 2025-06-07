module Downloader
  ( downloadImage,
    safeHttpLBS,
    safeWriteFile,
  )
where

import Control.Exception (SomeException, catch)
import Data.ByteString.Lazy as LBS
import Network.HTTP.Simple
import System.FilePath (takeFileName, (</>))
import System.IO (hPutStrLn, stderr)

-- | Internal function to perform the actual download
downloadImage :: String -> FilePath -> IO (Either String ())
downloadImage url savePath = do
  imageResponseResult <- safeHttpLBS url
  case imageResponseResult of
    Left err -> return $ Left err
    Right response -> do
      let fileName = takeFileName url
      let filePath = savePath </> fileName
      writeResult <- safeWriteFile filePath (getResponseBody response)
      case writeResult of
        Left err -> do
          hPutStrLn stderr $ "Failed to write file " ++ filePath ++ ": " ++ err
          return $ Left err
        Right _ -> do
          putStrLn $ "  Saved: " ++ filePath
          return $ Right ()

safeWriteFile :: FilePath -> LBS.ByteString -> IO (Either String ())
safeWriteFile filePath content =
  catch (Right () <$ LBS.writeFile filePath content) handleException
  where
    handleException :: SomeException -> IO (Either String ())
    handleException e = return $ Left $ show e

safeHttpLBS :: String -> IO (Either String (Response LBS.ByteString))
safeHttpLBS url = do
  catch (Right <$> httpLBS (parseRequest_ url)) handleException
  where
    handleException :: SomeException -> IO (Either String (Response LBS.ByteString))
    handleException e = return $ Left $ show e