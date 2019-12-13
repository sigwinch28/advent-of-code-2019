import qualified FrontPanel

import Data.Functor ((<&>))
import qualified Parse

import qualified Data.HashSet as Set
import qualified Data.HashMap.Strict as Map

defaultFileName = "data/day03.txt"

main = Parse.defaultMain defaultFileName run

load fileName = do
  contents <- readFile fileName
  let [wireOne, wireTwo] = map ((map FrontPanel.parseWire) . Parse.commaDelimited) $ lines contents in
    return (wireOne, wireTwo)

run 1 fileName = load fileName >>= (print . (uncurry taskOne))
run 2 fileName = load fileName >>= (print . (uncurry taskTwo))
run _ fileName = putStrLn "Task number out of range"

taskOne a b =
  let aCoords = Map.keysSet $ FrontPanel.plotAll a
      bCoords = Map.keysSet $ FrontPanel.plotAll b
      intersections = Set.toList $ Set.intersection aCoords bCoords
      distances = map FrontPanel.manhattanDistance intersections in
    minimum distances

taskTwo a b =
  let aPlot = FrontPanel.plotAll a
      bPlot = FrontPanel.plotAll b
      intersections = Map.intersectionWith (+) aPlot bPlot in
    minimum $ Map.elems intersections
