module Args
  ( SpiderOptions (..),
    parseSpiderOptions,
    getAppConfig,
  )
where

import Data.Maybe (fromMaybe)
import Options.Applicative
import System.FilePath (addTrailingPathSeparator)
import Types (AppConfig (..))

-- | Command line options
data SpiderOptions = SpiderOptions
  { optUrl :: String,
    optRecursive :: Bool,
    optLevel :: Maybe Int,
    optPath :: Maybe FilePath
  }
  deriving (Show)

-- | Default values
defaultLevel :: Int
defaultLevel = 5

defaultPath :: FilePath
defaultPath = "./data/"

defaultExtensions :: [String]
defaultExtensions = [".jpg", ".jpeg", ".png", ".gif", ".bmp"]

-- | Parser for spider options
spiderOptionsParser :: Parser SpiderOptions
spiderOptionsParser =
  SpiderOptions
    <$> strArgument
      ( metavar "URL"
          <> help "URL of the website to scrape"
      )
    <*> switch
      ( long "recursive"
          <> short 'r'
          <> help "Recursively download images"
      )
    <*> optional
      ( option
          auto
          ( long "level"
              <> short 'l'
              <> metavar "N"
              <> help
                ( "Maximum depth level for recursive download. "
                    ++ "Default is "
                    ++ show defaultLevel
                    ++ " when -r is specified."
                )
          )
      )
    <*> optional
      ( strOption
          ( long "path"
              <> short 'p'
              <> metavar "PATH"
              <> help
                ( "Path to save downloaded files. "
                    ++ "Default is \""
                    ++ defaultPath
                    ++ "\""
                )
          )
      )

-- | Parse command line options
parseSpiderOptions :: IO SpiderOptions
parseSpiderOptions = execParser opts
  where
    opts =
      info
        (spiderOptionsParser <**> helper)
        ( fullDesc
            <> progDesc "Spider: Extract images from websites recursively"
            <> header "spider - Web image scraper"
        )

-- | Convert parsed options to application configuration
getAppConfig :: SpiderOptions -> AppConfig
getAppConfig opts =
  AppConfig
    { cfgUrl = optUrl opts,
      cfgRecursive = optRecursive opts,
      cfgLevel = determineLevel (optRecursive opts) (optLevel opts),
      cfgPath = ensureTrailingSlash $ fromMaybe defaultPath (optPath opts),
      cfgExtensions = defaultExtensions
    }
  where
    determineLevel :: Bool -> Maybe Int -> Int
    determineLevel isRecursive maybeLvl =
      if isRecursive
        then fromMaybe defaultLevel maybeLvl
        else 1 -- Non-recursive: process only the current page
    ensureTrailingSlash :: FilePath -> FilePath
    ensureTrailingSlash = addTrailingPathSeparator