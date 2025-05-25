module Core
  ( crawl,
  )
where

import Control.Monad (forM_, when)
import Data.List (nub)
import Downloader
import HtmlParser
import System.IO (hPutStrLn, stderr)
import Text.HTML.TagSoup (Tag)
import Types

-- | Start the crawling process
crawl :: AppConfig -> IO ()
crawl config = do
  prepareDownloadDir (cfgPath config)
  let initialState = URLState (cfgUrl config) 1
  processUrl config emptyVisited (enqueue initialState emptyQueue)

-- | Process URLs from the queue until it's empty
processUrl :: AppConfig -> Visited -> Queue URLState -> IO ()
processUrl _ _ queue | isEmptyQueue queue = pure ()
processUrl config visited queue = case dequeue queue of
  Nothing -> pure () -- This shouldn't happen if we checked isEmptyQueue
  Just (state, remainingQueue) -> do
    let currentUrl = url state
    let currentDepth = depth state

    -- Skip if URL has already been visited
    if isVisited currentUrl visited
      then processUrl config visited remainingQueue
      else do
        -- Mark URL as visited
        let newVisited = addVisited currentUrl visited

        -- Fetch and process the page
        result <- fetchHtml currentUrl
        case result of
          Left err -> do
            hPutStrLn stderr $ "Error processing " ++ currentUrl ++ ": " ++ err
            processUrl config newVisited remainingQueue
          Right html -> do
            putStrLn $ "Processing: " ++ currentUrl ++ " (depth: " ++ show currentDepth ++ ")"

            -- Parse HTML
            let tags = parseHtml html

            -- Extract and download images
            let imageUrls = extractImages currentUrl (cfgExtensions config) tags
            forM_ imageUrls $ \imgUrl -> do
              downloadResult <- downloadImage (cfgPath config) imgUrl
              case downloadResult of
                Left err -> hPutStrLn stderr $ "Error downloading " ++ imgUrl ++ ": " ++ err
                Right path -> putStrLn $ "Downloaded: " ++ path

            -- If we haven't reached max depth and recursive option is on, add links to queue
            let newQueue =
                  if cfgRecursive config && currentDepth < cfgLevel config
                    then addLinksToQueue currentDepth tags currentUrl remainingQueue
                    else remainingQueue

            -- Continue with next URL
            processUrl config newVisited newQueue

-- | Add links from the current page to the queue
addLinksToQueue :: Int -> [Tag String] -> String -> Queue URLState -> Queue URLState
addLinksToQueue currentDepth tags baseUrl queue =
  let nextDepth = currentDepth + 1
      links = nub $ extractLinks baseUrl tags
      states = map (\u -> URLState u nextDepth) links
   in foldr enqueue queue states