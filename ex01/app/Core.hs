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
  imageUrls <- collectImageUrls config.url config.level 0
  let uniqueImageUrls = nubUrls imageUrls
  forM_ uniqueImageUrls $ \imgUrl ->
    putStrLn ("  Downloading image: " ++ imgUrl)
      >> downloadImage imgUrl config.path

-- | Recursively collect image URLs up to specified depth
collectImageUrls :: String -> Int -> Int -> IO [String]
collectImageUrls url maxDepth currentDepth
  | currentDepth > maxDepth = return []
  | currentDepth == maxDepth = do
      putStrLn ("Collecting image URLs from: " ++ url ++ " (depth " ++ show currentDepth ++ ")")
      extractImageUrls extensions url
  | otherwise = do
      putStrLn $ "Collecting image URLs from: " ++ url ++ " (depth " ++ show currentDepth ++ ")"
      imageUrlsInCurrentPage <- extractImageUrls extensions url
      children <- extractLinks url
      let uniqueChildren = nubUrls children
      imageUrlsInDescendants <- concat <$> mapM (\link -> collectImageUrls link maxDepth (currentDepth + 1)) uniqueChildren
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