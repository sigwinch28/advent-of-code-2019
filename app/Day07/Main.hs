import qualified Intcode

import Data.Functor ((<&>))
import qualified Data.List as List

import qualified Parse

defaultFileName = "data/day07.txt"

main :: IO ()
main = Parse.defaultMain defaultFileName run

load fileName = (readFile fileName) <&> ((map read) . Parse.commaDelimited)

run 1 fileName = load fileName >>= (print . taskOne)
run 2 fileName = load fileName >>= (print . taskTwo)
run _ fileName = putStrLn "Task number out of range"

taskOne prog = maximum $ map runAmps perms
  where runAmps phases = head $ foldl (\i n -> run (n:i)) [0] phases
        run inputs = Intcode.runProg inputs prog
        perms = List.permutations [0..4]

taskTwo prog = maximum $ map runAmps perms
  where perms = List.permutations [5..9]
        runAmps [p0,p1,p2,p3,p4] = last outE
          where outA = Intcode.runProg (p0:0:outE) prog
                outB = Intcode.runProg (p1:outA)   prog
                outC = Intcode.runProg (p2:outB)   prog
                outD = Intcode.runProg (p3:outC)   prog
                outE = Intcode.runProg (p4:outD)   prog
