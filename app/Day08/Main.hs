import qualified SpaceImage

import qualified System.Environment as Env
import Data.Functor ((<&>))
import Data.List (minimumBy)
import Data.Ord (comparing)

defaultSize = (25,6)
defaultFileName = "data/day08.txt"


main = do
  args <- Env.getArgs
  case args of
    [n,fileName,width,height] -> run (read n) fileName (read width) (read height)
    [n] -> (uncurry (run (read n) defaultFileName)) defaultSize
    _ -> do
      putStrLn "Task 1:"
      (uncurry (run 1 defaultFileName)) defaultSize
      putStrLn "Task 2:"
      (uncurry (run 2 defaultFileName)) defaultSize

load fileName = (readFile fileName) <&> (head . lines)

run 1 fileName width height = load fileName >>= (print . (taskOne width height))
run 2 fileName width height = load fileName >>= (putStr . (taskTwo width height))
run _ fileName width height = putStrLn "Task number out of range"

taskOne width height image = calcResult $ snd $ minimumBy (comparing fst) $ zip (map (countInstances SpaceImage.Black) layers) layers
  where layers = SpaceImage.parseImage width height image
        calcResult layer = (countInstances SpaceImage.White layer) * (countInstances SpaceImage.Transparent layer)
        countInstances pix layer = length $ concatMap (filter (== pix)) layer

taskTwo width height image = SpaceImage.showLayer $ SpaceImage.flattenImage $ SpaceImage.parseImage width height image
