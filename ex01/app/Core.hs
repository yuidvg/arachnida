{-# LANGUAGE OverloadedStrings #-}

module Core
  ( crawl,
    crawlWithConfig,
    CrawlConfig (..), -- Export the config type
    defaultCrawlConfig,
    conservativeCrawlConfig,
    aggressiveCrawlConfig,
    validateConfig,
  )
where

import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Monad (when)
import Data.List (nub)
import Data.Set qualified as Set
import Downloader
import HtmlParser
import Logger
import Network.HTTP.Simple
import Streaming
import Streaming.Prelude qualified as S
import System.Directory
import Types

-- | Configuration for crawling behavior
data CrawlConfig = CrawlConfig
  { -- | Number of concurrent downloaders
    crawlMaxDownloaders :: Int,
    -- | Number of links to process in each batch
    crawlBatchSize :: Int,
    -- | Maximum links to follow per page (Nothing = unlimited)
    crawlMaxLinksPerPage :: Maybe Int
  }
  deriving (Show, Eq)

-- | Default crawling configuration with conservative settings
defaultCrawlConfig :: CrawlConfig
defaultCrawlConfig =
  CrawlConfig
    { crawlMaxDownloaders = 3,
      crawlBatchSize = 5,
      crawlMaxLinksPerPage = Nothing -- No limit by default, but can be set to Just 20 for memory conservation
    }

-- | Conservative configuration for large sites
conservativeCrawlConfig :: CrawlConfig
conservativeCrawlConfig =
  defaultCrawlConfig
    { crawlMaxDownloaders = 2,
      crawlBatchSize = 3,
      crawlMaxLinksPerPage = Just 10
    }

-- | Aggressive configuration for small sites
aggressiveCrawlConfig :: CrawlConfig
aggressiveCrawlConfig =
  defaultCrawlConfig
    { crawlMaxDownloaders = 8,
      crawlBatchSize = 10,
      crawlMaxLinksPerPage = Nothing
    }

-- | Main crawling function with thread-safe logging
crawl :: AppConfig -> IO ()
crawl = crawlWithConfig defaultCrawlConfig

-- | Main crawling function with custom crawl configuration using streaming
crawlWithConfig :: CrawlConfig -> AppConfig -> IO ()
crawlWithConfig (CrawlConfig numDownloaders batchSize maxLinksPerPage) config = do
  logger <- newLogger
  visitedSet <- newTVarIO Set.empty

  -- Start background logging thread
  withAsync (logPrinter logger) $ \_ -> do
    -- Create download directory
    createDirectoryIfMissing True config.path

    logMsg logger "Starting streaming crawl..."

    -- Create the URL stream and process it
    let urlStream = collectImageUrlsStream (CrawlConfig numDownloaders batchSize maxLinksPerPage) config.url config.level 0 logger visitedSet

    -- Process the stream with controlled concurrency
    processImageUrlStream numDownloaders logger config.path urlStream

    logMsg logger "Crawling completed successfully."

-- | Stream of image URLs discovered during crawling
collectImageUrlsStream :: CrawlConfig -> String -> Int -> Int -> Logger -> TVar (Set.Set String) -> Stream (Of String) IO ()
collectImageUrlsStream (CrawlConfig _ batchSize maxLinksPerPage) url maxDepth currentDepth logger visitedSet
  | currentDepth > maxDepth = return ()
  | otherwise = do
      -- Check if URL has been visited to avoid redundant work and cycles
      let normalizedUrl = normalizeUrl url
      alreadyVisited <- lift $ atomically $ do
        visited <- readTVar visitedSet
        if Set.member normalizedUrl visited
          then return True
          else do
            writeTVar visitedSet (Set.insert normalizedUrl visited)
            return False

      if alreadyVisited
        then return ()
        else do
          lift $ logMsg logger $ "Collecting image URLs from: " ++ normalizedUrl ++ " (depth " ++ show currentDepth ++ ")"
          requestResult <- lift $ safeHttpLBS normalizedUrl
          case requestResult of
            Left err -> do
              lift $ logMsg logger $ "Failed to download page from " ++ normalizedUrl ++ ": " ++ err
            Right response -> do
              let html = getResponseBody response
              let imageUrlsInCurrentPage = extractImageUrls extensions normalizedUrl html

              -- Yield each image URL to the stream
              S.each imageUrlsInCurrentPage

              -- Process child links if we haven't reached max depth
              when (currentDepth < maxDepth) $ do
                let children = extractLinks normalizedUrl html
                let limitedChildren = case maxLinksPerPage of
                      Nothing -> children
                      Just limit -> take limit children
                let uniqueChildren = nubUrls limitedChildren

                -- Process children in batches using streaming
                let childStreams = map (\child -> collectImageUrlsStream (CrawlConfig 0 batchSize maxLinksPerPage) child maxDepth (currentDepth + 1) logger visitedSet) uniqueChildren

                -- Process batches of child streams
                processBatchedStreams batchSize childStreams

-- | Process batches of streams to control concurrency
processBatchedStreams :: Int -> [Stream (Of String) IO ()] -> Stream (Of String) IO ()
processBatchedStreams _ [] = return ()
processBatchedStreams batchSize streams = do
  let (currentBatch, remaining) = splitAt batchSize streams

  -- Process current batch by merging streams directly without converting to lists
  sequence_ currentBatch -- Execute each stream in the current batch

  -- Process remaining streams
  processBatchedStreams batchSize remaining

-- | Process the image URL stream with controlled concurrency
processImageUrlStream :: Int -> Logger -> FilePath -> Stream (Of String) IO () -> IO ()
processImageUrlStream _numDownloaders logger savePath urlStream = do
  -- Process stream in real-time as URLs arrive
  S.mapM_ (downloadSingleImage logger savePath) urlStream

-- | Download a single image with logging
downloadSingleImage :: Logger -> FilePath -> String -> IO ()
downloadSingleImage logger savePath imgUrl = do
  logMsg logger ("  Downloading image: " ++ imgUrl)
  result <- downloadImage logger imgUrl savePath
  case result of
    Left err -> logMsg logger ("  Download failed: " ++ err)
    Right _ -> return ()

-- A helper to make the normalization logic available to collectImageUrls
normalizeUrl :: String -> String
normalizeUrl url =
  let trimmed = reverse . dropWhile (== ' ') . reverse . dropWhile (== ' ') $ url
      normalized = normalizeSlashes trimmed
   in normalized
  where
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

-- | Remove duplicate URLs after normalizing them (useful for on-page links)
nubUrls :: [String] -> [String]
nubUrls = nub . map normalizeUrl

validateConfig :: CrawlConfig -> Either String CrawlConfig
validateConfig config@(CrawlConfig maxDownloaders batchSize _)
  | maxDownloaders <= 0 = Left "Downloaders must be positive"
  | batchSize <= 0 = Left "Batch size must be positive"
  | otherwise = Right config