module Main (main) where

import Args (parseArgs)
import Control.Exception (SomeException, catch)
import Core (processFiles)
import System.Exit (exitFailure)
import Types (Args (..))

-- | Main entry point
main :: IO ()
main = do
  args <- parseArgs `catch` handleParseError
  processFiles args
  where
    handleParseError :: SomeException -> IO Args
    handleParseError _ = do
      putStrLn "Error: Invalid arguments"
      putStrLn "Usage: scorpion FILE1 [FILE2 ...]"
      putStrLn ""
      putStrLn "Supported file formats:"
      putStrLn "  - JPEG (.jpg, .jpeg)"
      putStrLn "  - PNG (.png)"
      putStrLn "  - GIF (.gif)"
      putStrLn "  - BMP (.bmp)"
      exitFailure