module Core
  ( crawl,
  )
where

import Control.Monad (forM_, when)
import Data.Set as Set
import Downloader (downloadImage)
import HtmlParser (extractImageUrls, extractLinks)
import System.Directory (createDirectoryIfMissing)
import Types (AppConfig (..))

-- | Main crawling function
crawl :: AppConfig -> IO ()
crawl config = do
  -- Create download directory
  createDirectoryIfMissing True (cfgPath config)

  -- Start crawling from the initial URL
  crawlUrls config Set.empty [cfgUrl config] 0

-- | Recursively crawl URLs up to the specified depth
crawlUrls :: AppConfig -> Set String -> [String] -> Int -> IO ()
crawlUrls _ _ [] _ = return ()
crawlUrls config visited urls currentDepth = do
  when (currentDepth <= cfgLevel config) $ do
    -- Process current level URLs
    newUrls <- concat <$> mapM (processUrl config visited) urls

    -- Continue recursively if enabled and not at max depth
    when (cfgRecursive config && currentDepth < cfgLevel config) $
      crawlUrls config (Set.union visited (Set.fromList urls)) newUrls (currentDepth + 1)

-- | Process a single URL: download images and extract links
processUrl :: AppConfig -> Set String -> String -> IO [String]
processUrl config visited url
  | Set.member url visited = return []
  | otherwise = do
      putStrLn $ "Processing: " ++ url

      -- Extract and download images
      imageUrls <- extractImageUrls url (cfgExtensions config)
      forM_ imageUrls $ \imgUrl -> do
        putStrLn $ "  Downloading image: " ++ imgUrl
        downloadImage imgUrl (cfgPath config)

      -- Extract links for recursive crawling
      if cfgRecursive config
        then extractLinks url
        else return []