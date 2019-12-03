module Main where

import Lib
import Parse

import Data.List as List
import Data.Functor ((<&>))
import System.IO
import qualified System.Environment as Env
import qualified Data.Set as Set
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)

import qualified Fuel
import qualified Intcode
import qualified FrontPanel

main :: IO ()
main = do
  args <- Env.getArgs
  case args of
    [dayRaw,taskRaw,filename] ->
      let (day, task) = (read dayRaw, read taskRaw) in
        dispatch day task filename
    [dayRaw,taskRaw] ->
      let (day, task) = (read dayRaw, read taskRaw)
          filename = defaultFilename day task in
        do
          putStrLn $ "Using default filename " ++ filename
          dispatch day task filename
    _ -> putStrLn "Usage: day task [input-filename.txt]"

openFileLazy :: String -> IO String
openFileLazy fileName = openFile fileName ReadMode >>= hGetContents


defaultFilename :: Int -> Int -> String
defaultFilename 1 _ = "data/fuel-calculations/module-masses.txt"
defaultFilename 2 _ = "data/intcode/gravity-assist.txt"
defaultFilename 3 _ = "data/fuel-management/front-panel.txt"

dispatch :: Int -> Int -> (String -> IO ())
dispatch 1 1 = dayOneTaskOne
dispatch 1 2 = dayOneTaskTwo
dispatch 2 1 = dayTwoTaskOne
dispatch 2 2 = dayTwoTaskTwo
dispatch 3 1 = dayThreeTaskOne
dispatch 3 2 = dayThreeTaskTwo
dispatch d t = \_ -> putStrLn $ "Unknown task: day " ++ (show d) ++ " task " ++ (show t)

--
-- Day One
--

dayOneParse fileName = (openFileLazy fileName) <&> ( (map read) . lines )

dayOneTaskOne' :: [Int] -> Int
dayOneTaskOne' masses = sum $ map Fuel.fuelForMass masses

dayOneTaskOne fileName = (dayOneParse fileName) >>= (putStrLn . show . dayOneTaskOne')

dayOneTaskTwo' :: [Int] -> Int
dayOneTaskTwo' masses = sum $ map Fuel.totalFuel masses

dayOneTaskTwo fileName = (dayOneParse fileName) >>= (putStrLn . show . dayOneTaskTwo')
--
-- Day Two
--

dayTwoParse fileName = (openFileLazy fileName) <&> ((map read) . commaDelimited)

dayTwoTaskOne' :: [Int] -> Int
dayTwoTaskOne' prog = Intcode.run prog

dayTwoTaskOne fileName = (dayTwoParse fileName) >>= (putStrLn . show . dayTwoTaskOne')

dayTwoTaskTwo' :: [Int] -> Maybe Int
dayTwoTaskTwo' prog =
  let subst = \noun verb p -> ((Intcode.setMemory 1 noun) . (Intcode.setMemory 2 verb)) p
      permute = \noun verb p -> (noun, verb, subst noun verb prog)
      permutations = concatMap (\noun -> map (\verb -> permute noun verb prog) [1..99]) [1..99]
      results = map (\(n,v,prog) -> (n,v,Intcode.run prog)) permutations
      correctProg = List.find (\(_,_,res) -> res == 19690720) results in
    fmap (\(noun,verb,_) -> ((100 * noun) + verb)) correctProg
    

dayTwoTaskTwo fileName = (dayTwoParse fileName) >>= (putStrLn . (maybe "no result" show) . dayTwoTaskTwo')

--
-- Day Three
--

dayThreeParse fileName = do
  contents <- openFileLazy fileName
  let [wireOne, wireTwo] = map ((map FrontPanel.parseWire) . commaDelimited) $ lines contents in
    return (wireOne, wireTwo)

dayThreeTaskOne' :: [FrontPanel.Wire] -> [FrontPanel.Wire] -> Int
dayThreeTaskOne' a b =
  let aCoords = Map.keysSet $ FrontPanel.plotAll a
      bCoords = Map.keysSet $ FrontPanel.plotAll b
      intersections = Set.toList $ Set.intersection aCoords bCoords
      distances = map FrontPanel.manhattanDistance intersections in
    minimum distances

dayThreeTaskOne fileName = (dayThreeParse fileName) >>= (putStrLn . show . (uncurry dayThreeTaskOne'))

dayThreeTaskTwo' :: [FrontPanel.Wire] -> [FrontPanel.Wire] -> Int
dayThreeTaskTwo' a b =
  let aPlot = FrontPanel.plotAll a
      bPlot = FrontPanel.plotAll b
      intersections = Map.intersectionWith (+) aPlot bPlot in
    minimum $ Map.elems intersections

dayThreeTaskTwo fileName = (dayThreeParse fileName) >>= (putStrLn . show . (uncurry dayThreeTaskTwo'))
