import qualified Intcode

import Data.Functor ((<&>))
import qualified Data.List as List

import qualified Parse

defaultFileName = "data/day09.txt"

main :: IO ()
main = Parse.defaultMain defaultFileName run

load fileName = (readFile fileName) <&> ((map read) . Parse.commaDelimited)

run 1 fileName = load fileName >>= (print . taskOne)
run 2 fileName = load fileName >>= (print . taskTwo)
run _ fileName = putStrLn "Task number out of range"

taskOne prog = last $ Intcode.runProg [1] prog

taskTwo prog = last $ Intcode.runProg [2] prog
