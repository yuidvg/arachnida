module Types
  ( AppConfig (..),
    SpiderOptions (..),
  )
where

-- | Configuration for the spider application
data AppConfig = AppConfig
  { -- | Starting URL to crawl
    cfgUrl :: String,
    -- | Whether to crawl recursively
    cfgRecursive :: Bool,
    -- | Maximum depth level
    cfgLevel :: Int,
    -- | Directory to save downloaded images
    cfgPath :: FilePath,
    -- | Image file extensions to download
    cfgExtensions :: [String]
  }
  deriving (Show, Eq)

-- | Command line options for the spider
data SpiderOptions = SpiderOptions
  { -- | URL to crawl
    optUrl :: String,
    -- | Recursive flag
    optRecursive :: Bool,
    -- | Maximum depth
    optLevel :: Maybe Int,
    -- | Download path
    optPath :: Maybe String
  }
  deriving (Show, Eq)