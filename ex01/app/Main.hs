module Main (main) where

import Args (getAppConfig, parseSpiderOptions)
import Control.Exception (SomeException, try)
import Core (crawl)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)
import Types (AppConfig (..))

main :: IO ()
main = do
  -- Parse command line arguments
  options <- parseSpiderOptions
  let config = getAppConfig options

  -- Print configuration
  putStrLn $ "Starting spider with configuration:"
  putStrLn $ "  URL: " ++ cfgUrl config
  putStrLn $ "  Recursive: " ++ (if cfgRecursive config then "Yes" else "No")
  putStrLn $ "  Max depth: " ++ show (cfgLevel config)
  putStrLn $ "  Save path: " ++ cfgPath config
  putStrLn $ "  Extensions: " ++ show (cfgExtensions config)

  -- Start crawling
  result <- try $ crawl config
  case result of
    Left e -> do
      hPutStrLn stderr $ "Fatal error: " ++ show (e :: SomeException)
      exitFailure
    Right _ -> putStrLn "Spider completed successfully."