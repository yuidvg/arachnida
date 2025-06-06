module Main (main) where

import Args (getAppConfig, parseSpiderOptions)
import Control.Exception (SomeException, try)
import Core (crawl)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)
import Types (AppConfig (..), extensions)

main :: IO ()
main =
  do
    -- Parse command line arguments
    options <- parseSpiderOptions
    let config = getAppConfig options

    -- Print configuration
    putStrLn "Starting spider with configuration:"
    putStrLn $ "  URL: " ++ config.url
    putStrLn $ "  Recursive: " ++ (if config.level > 0 then "Yes" else "No")
    putStrLn $ "  Max depth: " ++ show config.level
    putStrLn $ "  Save path: " ++ config.path
    putStrLn $ "  Extensions: " ++ show extensions

    -- Start crawling
    result <- try $ crawl config
    case result of
      Left e -> do
        hPutStrLn stderr $ "Fatal error: " ++ show (e :: SomeException)
        exitFailure
      Right _ -> putStrLn "Spider completed successfully."
