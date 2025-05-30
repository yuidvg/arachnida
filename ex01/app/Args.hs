module Args
  ( getAppConfig,
    parseSpiderOptions,
  )
where

import Data.Maybe (fromMaybe)
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
      ( short 'r'
          <> help "Enable recursive crawling"
      )
    <*> optional
      ( option
          auto
          ( short 'l'
              <> metavar "N"
              <> help "Maximum depth level for recursive crawling (default: 5)"
          )
      )
    <*> optional
      ( strOption
          ( short 'p'
              <> metavar "PATH"
              <> help "Directory to save downloaded images (default: ./data/)"
          )
      )

-- | Convert SpiderOptions to AppConfig with defaults
getAppConfig :: SpiderOptions -> AppConfig
getAppConfig opts =
  AppConfig
    { url = opts.url,
      level = if opts.recursive then fromMaybe 5 opts.level else 1,
      path = fromMaybe "./data/" opts.path
    }
