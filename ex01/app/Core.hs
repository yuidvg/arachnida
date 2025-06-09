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
import Control.Monad (replicateM_, when)
import Data.List (nub)
import Data.Set qualified as Set
import Downloader
import HtmlParser
import Logger
import Network.HTTP.Simple
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
crawl = crawlWithConfig aggressiveCrawlConfig

-- | Main crawling function with custom crawl configuration
crawlWithConfig :: CrawlConfig -> AppConfig -> IO ()
crawlWithConfig (CrawlConfig numDownloaders batchSize maxLinksPerPage) config = do
  logger <- newLogger
  urlQueue <- newTQueueIO
  visitedSet <- newTVarIO Set.empty

  -- Start background logging thread
  withAsync (logPrinter logger) $ \_ -> do
    -- Create download directory
    createDirectoryIfMissing True config.path

    -- Producer thread: crawls the site and enqueues image URLs
    let producer = do
          collectImageUrls (CrawlConfig numDownloaders batchSize maxLinksPerPage) config.url config.level 0 logger urlQueue visitedSet
          -- After crawling is done, send termination signals to all downloaders
          replicateM_ numDownloaders $ atomically $ writeTQueue urlQueue Nothing
          logMsg logger "Crawling completed, waiting for downloads to finish..."

    -- Consumer (downloader) logic with proper exception handling
    let downloader = do
          let loop = do
                maybeUrl <- atomically $ readTQueue urlQueue
                case maybeUrl of
                  Nothing -> return () -- Graceful termination
                  Just imgUrl -> do
                    logMsg logger ("  Downloading image: " ++ imgUrl)
                    result <- downloadImage logger imgUrl config.path
                    case result of
                      Left err -> logMsg logger ("  Download failed: " ++ err)
                      Right _ -> return ()
                    loop
          loop

    -- Run producer and consumers concurrently
    withAsync producer $ \_ -> do
      -- Use mapConcurrently_ for controlled parallel downloading
      mapConcurrently_ (const downloader) [1 .. numDownloaders]

-- | Recursively collect image URLs up to specified depth with thread-safe logging
collectImageUrls :: CrawlConfig -> String -> Int -> Int -> Logger -> TQueue (Maybe String) -> TVar (Set.Set String) -> IO ()
collectImageUrls (CrawlConfig _ batchSize maxLinksPerPage) url maxDepth currentDepth logger queue visitedSet
  | currentDepth > maxDepth = return ()
  | otherwise = do
      -- Check if URL has been visited to avoid redundant work and cycles
      let normalizedUrl = normalizeUrl url
      alreadyVisited <- atomically $ do
        visited <- readTVar visitedSet
        if Set.member normalizedUrl visited
          then return True
          else do
            writeTVar visitedSet (Set.insert normalizedUrl visited)
            return False

      if alreadyVisited
        then return ()
        else do
          logMsg logger $ "Collecting image URLs from: " ++ normalizedUrl ++ " (depth " ++ show currentDepth ++ ")"
          requestResult <- safeHttpLBS normalizedUrl
          case requestResult of
            Left err -> do
              logMsg logger $ "Failed to download page from " ++ normalizedUrl ++ ": " ++ err
            Right response -> do
              let html = getResponseBody response
              let imageUrlsInCurrentPage = extractImageUrls extensions normalizedUrl html
              -- Enqueue found image URLs for downloaders
              atomically $ mapM_ (writeTQueue queue . Just) imageUrlsInCurrentPage

              when (currentDepth < maxDepth) $ do
                let children = extractLinks normalizedUrl html
                let limitedChildren = case maxLinksPerPage of
                      Nothing -> children
                      Just limit -> take limit children
                let uniqueChildren = nubUrls limitedChildren
                -- Process children with limited concurrency to avoid resource exhaustion
                let processInBatches [] = return ()
                    processInBatches batch = do
                      let (currentBatch, remaining) = splitAt batchSize batch
                      mapConcurrently_ (\child -> collectImageUrls (CrawlConfig 0 batchSize maxLinksPerPage) child maxDepth (currentDepth + 1) logger queue visitedSet) currentBatch
                      processInBatches remaining
                processInBatches uniqueChildren

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