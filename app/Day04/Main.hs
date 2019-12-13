import qualified FuelDepot

import qualified System.Environment as Env

defaultRange = (278384,842795)

main = do
  args <- Env.getArgs
  case args of
    [n,start,end] -> run (read n) (read start) (read end)
    [n] -> (uncurry (run (read n))) defaultRange
    _ -> do
      putStrLn "Task 1:"
      (uncurry (run 1)) defaultRange
      putStrLn "Task 2:"
      (uncurry (run 2)) defaultRange

run 1 start end = print $ taskOne start end
run 2 start end = print $ taskTwo start end
run _ start end = putStrLn "Task number out of range"

taskOne start end = length $ filter FuelDepot.groupCriteria (FuelDepot.passwordsInRange start end)
taskTwo start end = length $ filter FuelDepot.enhancedGroupCriteria (FuelDepot.passwordsInRange start end)

