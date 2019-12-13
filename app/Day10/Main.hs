import qualified Asteroid

import Data.Functor ((<&>))

import qualified Parse

defaultFileName = "data/day10.txt"

main :: IO ()
main = Parse.defaultMain defaultFileName run

load fileName = (readFile fileName) <&> Asteroid.parseMap

run 1 fileName = load fileName >>= (print . taskOne)
run 2 fileName = load fileName >>= (print . taskTwo)
run _ fileName = putStrLn "Task number out of range"

taskOne m = snd $ Asteroid.maxVisibility m

taskTwo m = (100*x) + y
  where station = fst $ Asteroid.maxVisibility m
        (x,y) = (Asteroid.vaporise station m) !! (200-1)
