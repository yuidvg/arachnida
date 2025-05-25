module Types
  ( AppConfig (..),
    URLState (..),
    Queue (..),
    emptyQueue,
    enqueue,
    dequeue,
    isEmptyQueue,
    Visited,
    emptyVisited,
    addVisited,
    isVisited,
  )
where

import qualified Data.Sequence as Seq
import qualified Data.Set as Set

-- | Application configuration
data AppConfig = AppConfig
  { -- | Starting URL
    cfgUrl :: String,
    -- | Whether to recursively download
    cfgRecursive :: Bool,
    -- | Maximum depth level for recursive download
    cfgLevel :: Int,
    -- | Path to save downloaded files
    cfgPath :: FilePath,
    -- | File extensions to download
    cfgExtensions :: [String]
  }
  deriving (Show)

-- | URL state for crawling
data URLState = URLState
  { -- | The URL to process
    url :: String,
    -- | Current depth level
    depth :: Int
  }
  deriving (Show, Eq, Ord)

-- | Queue implementation using Data.Sequence
newtype Queue a = Queue (Seq.Seq a)
  deriving (Show)

-- | Create an empty queue
emptyQueue :: Queue a
emptyQueue = Queue Seq.empty

-- | Add an item to the queue
enqueue :: a -> Queue a -> Queue a
enqueue x (Queue q) = Queue (q Seq.|> x)

-- | Remove an item from the queue
dequeue :: Queue a -> Maybe (a, Queue a)
dequeue (Queue q) = case Seq.viewl q of
  Seq.EmptyL -> Nothing
  x Seq.:< xs -> Just (x, Queue xs)

-- | Check if queue is empty
isEmptyQueue :: Queue a -> Bool
isEmptyQueue (Queue q) = Seq.null q

-- | Set of visited URLs
type Visited = Set.Set String

-- | Create an empty visited set
emptyVisited :: Visited
emptyVisited = Set.empty

-- | Add a URL to the visited set
addVisited :: String -> Visited -> Visited
addVisited = Set.insert

-- | Check if a URL has been visited
isVisited :: String -> Visited -> Bool
isVisited = Set.member