module Args
  ( getAppConfig,
    parseSpiderOptions,
  )
where

import Options.Applicative
import Types (AppConfig (..), SpiderOptions (..))

-- | Parse command line options
parseSpiderOptions :: IO SpiderOptions
parseSpiderOptions = execParser opts
  where
    opts =
      info
        (spiderOptionsParser <**> helper)
        ( fullDesc
            <> progDesc "Extract images from websites recursively"
            <> header "spider - a web image extractor"
        )

-- | Command line options parser
spiderOptionsParser :: Parser SpiderOptions
spiderOptionsParser =
  SpiderOptions
    <$> strArgument
      ( metavar "URL"
          <> help "URL to start crawling from"
      )
    <*> switch
      ( long "recursive"
          <> short 'r'
          <> help "Enable recursive crawling"
      )
    <*> optional
      ( option
          auto
          ( long "level"
              <> short 'l'
              <> metavar "N"
              <> help "Maximum depth level for recursive crawling (default: 5)"
          )
      )
    <*> optional
      ( strOption
          ( long "path"
              <> short 'p'
              <> metavar "PATH"
              <> help "Directory to save downloaded images (default: ./data/)"
          )
      )

-- | Convert SpiderOptions to AppConfig with defaults
getAppConfig :: SpiderOptions -> AppConfig
getAppConfig opts =
  AppConfig
    { cfgUrl = optUrl opts,
      cfgRecursive = optRecursive opts,
      cfgLevel = maybe 5 id (optLevel opts),
      cfgPath = maybe "./data/" id (optPath opts),
      cfgExtensions = [".jpg", ".jpeg", ".png", ".gif", ".bmp", ".webp"]
    }