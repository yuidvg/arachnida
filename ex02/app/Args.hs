module Args (parseArgs) where

import Options.Applicative
import Types (Args (..))

-- | Parse command line arguments
parseArgs :: IO Args
parseArgs = execParser opts
  where
    opts =
      info
        (argsParser <**> helper)
        ( fullDesc
            <> progDesc "Extract and display EXIF and metadata from image files"
            <> header "scorpion - image metadata extractor"
        )

-- | Parser for command line arguments
argsParser :: Parser Args
argsParser =
  Args
    <$> some
      ( argument
          str
          ( metavar "FILE..."
              <> help "Image files to analyze"
          )
      )