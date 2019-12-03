module Main where

import Lib
import Parse

import Data.List (find)
import System.IO

import qualified Fuel
import qualified Intcode
import qualified FrontPanel

main :: IO ()
main = someFunc

dayOneTaskOne fileName = do
  handle <- openFile fileName ReadMode
  contents <- hGetContents handle
  let moduleMasses = map read (lines contents)
      totalFuel  = sum (map Fuel.fuelForMass moduleMasses) in
    putStrLn (show totalFuel)

dayOneTaskTwo fileName = do
  handle <- openFile fileName ReadMode
  contents <- hGetContents handle
  let moduleMasses = map read (lines contents)
      totalFuel = sum (map Fuel.totalFuel moduleMasses) in
    putStrLn (show totalFuel)

dayTwoTaskOne fileName = do
  handle <- openFile fileName ReadMode
  contents <- hGetContents handle
  let prog = map read (commaDelimited contents)
      addrZero = Intcode.run prog in
    putStrLn (show addrZero)

dayTwoTaskTwo fileName = do
  handle <- openFile fileName ReadMode
  contents <- hGetContents handle
  let prog = map read (commaDelimited contents)
      subst = \noun verb p -> ((Intcode.setMemory 1 noun) . (Intcode.setMemory 2 verb)) p
      permute = \noun verb p -> (noun, verb, subst noun verb prog)
      permutations = concatMap (\noun -> map (\verb -> permute noun verb prog) [1..99]) [1..99]
      results = map (\(n,v,prog) -> (n,v,Intcode.run prog)) permutations
      answer = find (\(_,_,res) -> res == 19690720) results in
    putStrLn $ maybe "no result" (\(noun,verb,_) -> show ((100 * noun) + verb)) answer

dayThreeTaskOne fileName = do
  handle <- openFile fileName ReadMode
  contents <- hGetContents handle
  let [wireOne,wireTwo] = map ( FrontPanel.plotAll . (map FrontPanel.parseWire) . commaDelimited ) $ lines contents in
    return (wireOne, wireTwo)
