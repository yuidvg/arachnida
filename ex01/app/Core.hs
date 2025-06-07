{-# LANGUAGE OverloadedStrings #-}

module Core
  ( crawl,
  )
where

import Control.Concurrent.Async
import Data.List (nub)
import Downloader
import HtmlParser
import Logger
import Network.HTTP.Simple
import System.Directory
import Types

-- | Main crawling function with thread-safe logging
crawl :: AppConfig -> IO ()
crawl config = do
  logger <- newLogger

  -- Start background logging thread
  withAsync (logPrinter logger) $ \_ -> do
    -- Create download directory
    createDirectoryIfMissing True config.path
    imageUrls <- collectImageUrls config.url config.level 0 logger
    let uniqueImageUrls = nubUrls imageUrls
    mapConcurrently_
      ( \imgUrl -> do
          logMsg logger ("  Downloading image: " ++ imgUrl)
          downloadImage logger imgUrl config.path
      )
      uniqueImageUrls

-- | Recursively collect image URLs up to specified depth with thread-safe logging
collectImageUrls :: String -> Int -> Int -> Logger -> IO [String]
collectImageUrls url maxDepth currentDepth logger
  | currentDepth > maxDepth = return []
  | otherwise = do
      logMsg logger $ "Collecting image URLs from: " ++ url ++ " (depth " ++ show currentDepth ++ ")"
      requestResult <- safeHttpLBS url
      case requestResult of
        Left err -> do
          logMsg logger $ "Failed to download page from " ++ url ++ ": " ++ err
          return []
        Right response -> do
          let html = getResponseBody response
          let imageUrlsInCurrentPage = extractImageUrls extensions url html
          if currentDepth == maxDepth
            then return imageUrlsInCurrentPage
            else do
              let children = extractLinks url html
              let uniqueChildren = nubUrls children
              imageUrlsInDescendants <- concat <$> mapConcurrently (\child -> collectImageUrls child maxDepth (currentDepth + 1) logger) uniqueChildren
              return (imageUrlsInCurrentPage ++ imageUrlsInDescendants)

-- | Remove duplicate URLs after normalizing them
nubUrls :: [String] -> [String]
nubUrls = nub . map normalizeUrl
  where
    normalizeUrl :: String -> String
    normalizeUrl url =
      let trimmed = reverse . dropWhile (== ' ') . reverse . dropWhile (== ' ') $ url
          normalized = normalizeSlashes trimmed
       in normalized

    normalizeSlashes :: String -> String
    normalizeSlashes [] = []
    normalizeSlashes [x] = [x]
    normalizeSlashes ('h' : 't' : 't' : 'p' : ':' : '/' : '/' : rest) =
      "http://" ++ normalizeSlashes' rest
    normalizeSlashes ('h' : 't' : 't' : 'p' : 's' : ':' : '/' : '/' : rest) =
      "https://" ++ normalizeSlashes' rest
    normalizeSlashes (x : xs) = x : normalizeSlashes xs

    normalizeSlashes' :: String -> String
    normalizeSlashes' [] = []
    normalizeSlashes' [x] = [x]
    normalizeSlashes' ('/' : '/' : xs) = normalizeSlashes' ('/' : xs)
    normalizeSlashes' (x : xs) = x : normalizeSlashes' xs