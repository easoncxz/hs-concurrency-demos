module Lib
  ( someFunc
  ) where

import Control.Concurrent (forkIO, myThreadId, threadDelay, ThreadId)
import Control.Concurrent.Async (mapConcurrently)
import System.Random (randomIO)

-- | Interesting how essentially here we called `fork` but
-- didn't need to explicitly call anything equivalent to `join`.
-- Looks like `mapConcurrently` returns an action that doesn't finish
-- until all threads inside have finished.
autojoinDemo :: IO ()
autojoinDemo = do
  _ :: ThreadId <- fireAndForget
  _ids :: [ThreadId] <- mapConcurrently (\_ -> waitAndPrint) [1..10]
  putStrLn "All forked."
  where
    waitAndPrint :: IO ThreadId
    waitAndPrint = do
      seconds :: Double <- randomIO
      threadDelay (round (seconds * 1000 * 1000))
      i <- myThreadId
      putStrLn (show i)
      return i
    fireAndForget :: IO ThreadId
    fireAndForget = forkIO $ do
      threadDelay (4 * 1000 * 1000)
      putStrLn "Now we're done, phew."

someFunc :: IO ()
someFunc = putStrLn "someFunc"
