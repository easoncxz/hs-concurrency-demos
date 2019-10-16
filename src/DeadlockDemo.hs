{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}


module DeadlockDemo where


import Control.Concurrent (myThreadId, threadDelay)
import Control.Concurrent.MVar (MVar, newMVar, takeMVar, putMVar)
import Control.Concurrent.Async (mapConcurrently)
import Control.Monad (replicateM, mapM_)

-- | A chopstick is something only one person (i.e. thread) can
-- pick up at a time.
type Chopstick = MVar ()

-- | A diner is a person (i.e. thread) that needs to use all chopsticks
-- at once in order to eat. (I know, terrible analogy; I'm Chinese myself.)
type Diner = [Chopstick] -> IO ()

deadlockDiner :: Diner
deadlockDiner cs = do
  -- non-deterministic lock-acquisition order!
  _ <- mapConcurrently takeMVar cs
  i <- myThreadId
  putStrLn ("Now we eat! (I am: " ++ show i ++ ")")
  _ <- mapConcurrently (\m -> putMVar m ()) cs
  return ()

nolockDiner :: Diner
nolockDiner cs = do
  mapM_ takeMVar cs -- effectively ordering the locks!
  i <- myThreadId
  putStrLn ("Now we eat! (I am: " ++ show i ++ ")")
  mapM_ (\mv -> putMVar mv ()) cs

runRestaurant :: Int -> Diner -> IO ()
runRestaurant seatCount diner = do
  chopsticks :: [Chopstick] <- replicateM 10 (newMVar ())
  _ <- mapConcurrently (\_ -> diner chopsticks) [1..seatCount]
  putStrLn "All diners have eaten!"

exampleRestaurant :: IO ()
exampleRestaurant = do
  putStrLn "No deadlock:" >> runRestaurant 100 nolockDiner
  putStrLn "---"
  putStrLn "Deadlock:" >> runRestaurant 100 deadlockDiner
