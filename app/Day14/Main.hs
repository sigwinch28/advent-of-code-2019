
import Data.Functor ((<&>))

import qualified Parse

defaultFileName = "data/day14.txt"

main :: IO ()
main = Parse.defaultMain defaultFileName run

load fileName = (readFile fileName) <&> ((map read) . Parse.commaDelimited)

run 1 fileName = load fileName >>= (print . taskOne)
run 2 fileName = load fileName >>= (print . taskTwo)
run _ fileName = putStrLn "Task number out of range"

taskOne :: [Int] -> String
taskOne _ = "Not implemented"

taskTwo :: [Int] -> String
taskTwo _ = "Not implemented"
