import qualified Intcode
import qualified HullPainter

import Data.Functor ((<&>))
import qualified Data.List as List
import qualified Data.Map.Strict as Map

import qualified Parse

defaultFileName = "data/day11.txt"

main :: IO ()
main = Parse.defaultMain defaultFileName run

load fileName = (readFile fileName) <&> ((map read) . Parse.commaDelimited)

run 1 fileName = load fileName >>= (print . taskOne)
run 2 fileName = load fileName >>= (putStr . taskTwo)
run _ fileName = putStrLn "Task number out of range"

taskOne prog = Map.size $ (\(_,_,hull) -> hull) $ snd $ last outRobot
  where outProg = Intcode.runProg (0: (map fst outRobot)) prog
        outRobot = HullPainter.run outProg

taskTwo prog = HullPainter.draw $ snd $ last outRobot
  where outProg = Intcode.runProg (1: (map fst outRobot)) prog
        outRobot = HullPainter.run outProg
