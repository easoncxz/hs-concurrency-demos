
import Control.Exception
  (BlockedIndefinitelyOnMVar(BlockedIndefinitelyOnMVar)
  , catch
  )

import RaceConditionDemo (exampleAccounting)
import DeadlockDemo (exampleRestaurant)

main :: IO ()
main = do
  exampleAccounting
  exampleRestaurant
