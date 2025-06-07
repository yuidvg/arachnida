module Logger
  ( Logger,
    newLogger,
    logMsg,
    logPrinter,
  )
where

import Control.Concurrent.STM
import Control.Monad (forever)

-- | Thread-safe logger using STM
newtype Logger = Logger (TQueue String)

-- | Create a new logger
newLogger :: IO Logger
newLogger = Logger <$> newTQueueIO

-- | Log a message thread-safely
logMsg :: Logger -> String -> IO ()
logMsg (Logger queue) msg = atomically $ writeTQueue queue msg

-- | Background thread to print log messages
logPrinter :: Logger -> IO ()
logPrinter (Logger queue) = forever $ do
  msg <- atomically $ readTQueue queue
  putStrLn msg