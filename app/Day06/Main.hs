import qualified Orbit

import Data.Functor ((<&>))

import qualified Parse

defaultFileName = "data/day06.txt"

main :: IO ()
main = Parse.defaultMain defaultFileName run

load fileName = (readFile fileName) <&> ((map Orbit.parseOrbit) . lines)

run 1 fileName = load fileName >>= (print . taskOne)
run 2 fileName = load fileName >>= (print . taskTwo)
run _ fileName = putStrLn "Task number out of range"

taskOne orbits = Orbit.totalOrbits "COM" orbits

taskTwo orbits = (\n -> n - 1) $ length $ Orbit.pathBetween "YOU" "SAN" orbits
