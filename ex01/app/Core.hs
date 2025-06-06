module Core
  ( crawl,
  )
where

import Control.Monad (forM_)
import Data.List (nub)
import Downloader (downloadImage)
import HtmlParser (extractImageUrls, extractLinks)
import System.Directory (createDirectoryIfMissing)
import Types (AppConfig (..), extensions)

-- | Main crawling function
crawl :: AppConfig -> IO ()
crawl config = do
  -- Create download directory
  createDirectoryIfMissing True config.path

  urls <- collectUrls config.url config.level 0
  let uniqueUrls = nubUrls urls
  imageUrls <- mapM (extractImageUrls extensions) uniqueUrls
  let uniqueImageUrls = nubUrls (concat imageUrls)

  -- download images
  forM_ uniqueImageUrls $ \imgUrl ->
    putStrLn ("  Downloading image: " ++ imgUrl)
      >> downloadImage imgUrl config.path

-- | Recursively collect URLs up to specified depth
collectUrls :: String -> Int -> Int -> IO [String]
collectUrls url maxDepth currentDepth
  | currentDepth > maxDepth = return []
  | otherwise = do
      putStrLn $ "Collecting URLs from: " ++ url ++ " (depth " ++ show currentDepth ++ ")"
      -- Get links from current page
      children <- extractLinks url
      let uniqueChildren = nubUrls children
      -- Recursively collect from children
      descendants <- concat <$> mapM (\link -> collectUrls link maxDepth (currentDepth + 1)) uniqueChildren
      let uniqueDescendants = nubUrls descendants
      -- Return current URL plus all descendants
      return (url : uniqueDescendants)

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