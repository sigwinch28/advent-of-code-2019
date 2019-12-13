import qualified Fuel

import Data.Functor ((<&>))
import qualified Parse

defaultFileName = "data/fuel-calculations/module-masses.txt"

main = Parse.defaultMain defaultFileName run

load fileName = (readFile fileName) <&> ( (map read) . lines )

run 1 fileName = load fileName >>= (print . taskOne)
run 2 fileName = load fileName >>= (print . taskTwo)
run _ fileName = putStrLn "Task number out of range"

taskOne masses = sum $ map Fuel.fuelForMass masses

taskTwo masses = sum $ map Fuel.totalFuel masses
