module Types
  ( AppConfig (..),
    SpiderOptions (..),
    extensions,
  )
where

-- | Configuration for the spider application
data AppConfig = AppConfig
  { -- | Starting URL to crawl
    url :: String,
    -- | Maximum depth level
    level :: Int,
    -- | Directory to save downloaded images
    path :: FilePath
  }
  deriving (Show, Eq)

-- | Command line options for the spider
data SpiderOptions = SpiderOptions
  { -- | URL to crawl
    url :: String,
    -- | Recursive flag
    recursive :: Bool,
    -- | Maximum depth
    level :: Maybe Int,
    -- | Download path
    path :: Maybe String
  }
  deriving (Show, Eq)

extensions :: [String]
extensions = [".jpg", ".jpeg", ".png", ".gif", ".bmp"]
