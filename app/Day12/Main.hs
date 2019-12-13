import qualified NBody
import qualified NBody.Parse

import Data.Functor ((<&>))

import qualified Parse

defaultFileName = "data/day12.txt"

main :: IO ()
main = Parse.defaultMain defaultFileName run

load fileName = (readFile fileName) <&> ((map NBody.Parse.parseBody) . lines)

run 1 fileName = load fileName >>= (print . taskOne)
run 2 fileName = load fileName >>= (print . taskTwo)
run _ fileName = putStrLn "Task number out of range"

taskOne bodies = sum $ map NBody.energy $ (flip (!!)) 1000 $ NBody.bodySteps bodies

taskTwo bodies = NBody.bodiesPeriod bodies
