{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module RaceConditionDemo where

import Control.DeepSeq (NFData)
import Data.IORef (IORef, modifyIORef, modifyIORef', atomicModifyIORef, atomicModifyIORef', newIORef, readIORef, writeIORef)
import Control.Concurrent.STM (STM, TVar, atomically, newTVar, readTVar, writeTVar, modifyTVar, modifyTVar')
import Control.Concurrent.MVar (MVar, newMVar, takeMVar, putMVar, modifyMVar_)
import qualified Control.Concurrent.MVar.Strict as Strict
import Control.Concurrent.Async (mapConcurrently)
import Control.Monad (replicateM_)

-- | Like a typeclass definition, but explicit
data RefInstance r m a =
  RefInstance
    { newRef :: a -> m (r a)
    , readRef :: r a -> m a
    , writeRef :: r a -> a -> m ()
    , modifyRef :: r a -> (a -> a) -> m ()
    , modifyRef' :: r a -> (a -> a) -> m ()
    }

-- | Like a typeclass instance, but explicit
iorefInstance :: RefInstance IORef IO a
iorefInstance = RefInstance
  { newRef = newIORef
  , readRef = readIORef
  , writeRef = writeIORef
  , modifyRef = modifyIORef     -- no one said this was atomic
  , modifyRef' = modifyIORef'   -- no one said this was atomic
  }

atomicIORefInstance :: RefInstance IORef IO a
atomicIORefInstance = RefInstance
  { newRef = newIORef
  , readRef = readIORef
  , writeRef = writeIORef
  , modifyRef = \r f -> atomicModifyIORef r (\v -> (f v, ()))
  , modifyRef' = \r f -> atomicModifyIORef' r (\v -> (f v, ()))
  }

mvarRefInstance :: RefInstance MVar IO a
mvarRefInstance = RefInstance
  { newRef = newMVar
  , readRef = takeMVar
  , writeRef = putMVar
  , modifyRef = \r f -> modifyMVar_ r (\v -> pure (f v))
  , modifyRef' = \r f -> modifyMVar_ r (\v -> pure (f v))
  }

strictMVarRefInstance :: NFData a => RefInstance Strict.MVar IO a
strictMVarRefInstance = RefInstance
  { newRef = Strict.newMVar
  , readRef = Strict.takeMVar
  , writeRef = Strict.putMVar
  , modifyRef = \r f -> Strict.modifyMVar_ r (\v -> pure (f v))
  , modifyRef' = \r f -> Strict.modifyMVar_ r (\v -> pure (f v))
  }

tvarRefInstance :: RefInstance TVar IO a
tvarRefInstance = RefInstance
  { newRef = \v -> atomically (newTVar v)
  , readRef = \r -> atomically $ readTVar r
  , writeRef = \r v -> atomically $ writeTVar r v
  , modifyRef = \r f -> atomically $ modifyTVar r f
  , modifyRef' = \r f -> atomically $ modifyTVar' r f
  }

doAccounting :: RefInstance r IO Int -> (Int, Int) -> IO ()
doAccounting RefInstance{newRef, readRef, modifyRef'} (threadCount, iterations) = do
  tally <- newRef 0
  let repeatedlyIncrement = replicateM_ iterations (modifyRef' tally (+1))
  _ <- mapConcurrently (\_ -> repeatedlyIncrement) [1..threadCount]
  finalCount <- readRef tally
  putStrLn ("All done. Final count: " ++ show finalCount)

-- | A redundant example of the same logic specialised to MVar's;
-- this is equivalent to the partially-applied `doAccounting mvarRefInstance`.
mvarAccounting :: (Int, Int) -> IO ()
mvarAccounting (threadCount, iterations) = do
  tally <- newMVar (0 :: Int)
  let repeatedlyIncrement =
          replicateM_
          iterations
          (modifyMVar_
            tally
            (\v -> pure (v + 1)))  -- MVar-specific API usage here
  _ <- mapConcurrently (\_ -> repeatedlyIncrement) [1..threadCount]
  finalCount <- takeMVar tally
  putStrLn ("All done. Final count: " ++ show finalCount)

exampleAccounting :: IO ()
exampleAccounting = do
  putStr "IORef... (race conditions here)\t"
  doAccounting iorefInstance (1000, 1000)
  putStr "IORef with atomicModifyIORef...\t"
  doAccounting atomicIORefInstance (1000, 1000)
  putStr "MVar...\t"
  doAccounting mvarRefInstance (1000, 1000)
  putStr "Strict MVar...\t"
  doAccounting strictMVarRefInstance (1000, 1000)
  putStr "TVar...\t"
  doAccounting tvarRefInstance (1000, 1000)



