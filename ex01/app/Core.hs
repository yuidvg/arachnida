module Core
  ( crawl,
  )
where

import Control.Monad (forM_, when)
import Data.Set as Set
import Downloader (downloadImage)
import HtmlParser (extractImageUrls, extractLinks)
import System.Directory (createDirectoryIfMissing)
import Types (AppConfig (..), extensions)

-- | Main crawling function
crawl :: AppConfig -> IO ()
crawl config = do
  -- Create download directory
  createDirectoryIfMissing True config.path

  -- Start crawling from the initial URL
  crawlUrls config Set.empty [config.url] 0

-- | Recursively crawl URLs up to the specified depth
crawlUrls :: AppConfig -> Set String -> [String] -> Int -> IO ()
crawlUrls _ _ [] _ = return ()
crawlUrls config visited urls currentDepth = do
  when (currentDepth <= config.level) $ do
    -- Process current level URLs
    newUrls <- concat <$> mapM (processUrl config visited) urls

    -- Continue recursively if enabled and not at max depth
    when (config.level > 1 && currentDepth < config.level) $
      crawlUrls config (Set.union visited (Set.fromList urls)) newUrls (currentDepth + 1)

-- | Process a single URL: download images and extract links
processUrl :: AppConfig -> Set String -> String -> IO [String]
processUrl config visited url
  | Set.member url visited = return []
  | otherwise = do
      putStrLn $ "Processing: " ++ url

      -- Extract and download images
      imageUrls <- extractImageUrls url extensions
      forM_ imageUrls $ \imgUrl -> do
        putStrLn $ "  Downloading image: " ++ imgUrl
        downloadImage imgUrl config.path

      -- Extract links for recursive crawling
      if config.level > 1
        then extractLinks url
        else return []