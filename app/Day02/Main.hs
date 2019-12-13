import qualified Intcode

import Data.Functor ((<&>))
import qualified Data.List as List
import Data.Maybe (fromJust)

import qualified Parse

defaultFileName = "data/day02.txt"

main :: IO ()
main = Parse.defaultMain defaultFileName run

load fileName = (readFile fileName) <&> ((map read) . Parse.commaDelimited)

run 1 fileName = load fileName >>= (print . taskOne)
run 2 fileName = load fileName >>= (print . taskTwo)
run _ fileName = putStrLn "Task number out of range"

taskOne prog = head $ fst $ Intcode.runMem (prog,0)

taskTwo prog = fromJust $ fmap (\(noun,verb,_) -> ((100 * noun) + verb)) correctProg
  where subst noun verb prog = head prog : ( [noun,verb] ++ (drop 3 prog))
        permute noun verb prog = (noun, verb, subst noun verb prog)
        permutations = concatMap (\noun -> map (\verb -> permute noun verb prog) [1..99]) [1..99]
        results = map (\(n,v,prog) -> (n,v, head $ fst $ Intcode.runMem (prog,0))) permutations
        correctProg = List.find (\(_,_,res) -> res == 19690720) results
