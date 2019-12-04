module Main where

import Lib
import Parse

import Data.List as List
import Data.Functor ((<&>))
import System.IO
import qualified System.Environment as Env
import qualified Data.HashSet as Set
import qualified Data.HashMap.Strict as Map
import Data.Maybe (fromMaybe)

import qualified Fuel
import qualified Intcode
import qualified FrontPanel
import qualified FuelDepot

main :: IO ()
main = do
  args <- Env.getArgs
  case args of
    (dayS:(taskS:rest)) -> dispatch (read dayS) (read taskS) rest
    _ -> putStrLn "Usage: day task [task args]"

openFileLazy :: String -> IO String
openFileLazy fileName = openFile fileName ReadMode >>= hGetContents

dispatch :: Int -> Int -> [String] -> IO ()
dispatch 1 1 = dayOneTaskOne
dispatch 1 2 = dayOneTaskTwo
dispatch 2 1 = dayTwoTaskOne
dispatch 2 2 = dayTwoTaskTwo
dispatch 3 1 = dayThreeTaskOne
dispatch 3 2 = dayThreeTaskTwo
dispatch 4 1 = dayFourTaskOne
dispatch 4 2 = dayFourTaskTwo
dispatch d t = \_ -> putStrLn $ "Unknown task: day " ++ (show d) ++ " task " ++ (show t)

--
-- Day One
--

dayOneDefaultArgs = ["data/fuel-calculations/module-masses.txt"]

dayOneParse fileName = (openFileLazy fileName) <&> ( (map read) . lines )

dayOneTaskOne' :: [Int] -> Int
dayOneTaskOne' masses = sum $ map Fuel.fuelForMass masses

dayOneTaskOne []         = dayOneTaskOne dayOneDefaultArgs
dayOneTaskOne [fileName] = (dayOneParse fileName) >>= (putStrLn . show . dayOneTaskOne')
dayOneTaskOne _          = putStrLn "Usage: 1 1 [filename]"

dayOneTaskTwo' :: [Int] -> Int
dayOneTaskTwo' masses = sum $ map Fuel.totalFuel masses

dayOneTaskTwo []         = dayOneTaskTwo dayOneDefaultArgs
dayOneTaskTwo [fileName] = (dayOneParse fileName) >>= (putStrLn . show . dayOneTaskTwo')
dayOneTaskTwo _          = putStrLn "Usage: 1 2 [filename]"
--
-- Day Two
--

dayTwoDefaultArgs = ["data/intcode/gravity-assist.txt"]

dayTwoParse fileName = (openFileLazy fileName) <&> ((map read) . commaDelimited)

dayTwoTaskOne' :: [Int] -> Int
dayTwoTaskOne' prog = Intcode.run prog

dayTwoTaskOne []         = dayTwoTaskOne dayTwoDefaultArgs
dayTwoTaskOne [fileName] = (dayTwoParse fileName) >>= (putStrLn . show . dayTwoTaskOne')
dayTwoTaskOne _          = putStrLn "Usage: 2 1 [filename]"

dayTwoTaskTwo' :: [Int] -> Maybe Int
dayTwoTaskTwo' prog =
  let subst noun verb prog = ((Intcode.setMemory 1 noun) . (Intcode.setMemory 2 verb)) prog
      permute noun verb prog = (noun, verb, subst noun verb prog)
      permutations = concatMap (\noun -> map (\verb -> permute noun verb prog) [1..99]) [1..99]
      results = map (\(n,v,prog) -> (n,v,Intcode.run prog)) permutations
      correctProg = List.find (\(_,_,res) -> res == 19690720) results in
    fmap (\(noun,verb,_) -> ((100 * noun) + verb)) correctProg

dayTwoTaskTwo []         = dayTwoTaskTwo dayTwoDefaultArgs
dayTwoTaskTwo [fileName] = (dayTwoParse fileName) >>= (putStrLn . (maybe "no result" show) . dayTwoTaskTwo')
dayTwoTaskTwo _          = putStrLn "Usage: 2 2 [filename]"


--
-- Day Three
--

dayThreeDefaultArgs = ["data/fuel-management/front-panel.txt"]

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

dayThreeTaskOne []         = dayThreeTaskOne dayThreeDefaultArgs
dayThreeTaskOne [fileName] = (dayThreeParse fileName) >>= (putStrLn . show . (uncurry dayThreeTaskOne'))
dayThreeTaskOne _          = putStrLn "Usage: 3 1 [filename]"

dayThreeTaskTwo' :: [FrontPanel.Wire] -> [FrontPanel.Wire] -> Int
dayThreeTaskTwo' a b =
  let aPlot = FrontPanel.plotAll a
      bPlot = FrontPanel.plotAll b
      intersections = Map.intersectionWith (+) aPlot bPlot in
    minimum $ Map.elems intersections

dayThreeTaskTwo :: [String] -> IO ()
dayThreeTaskTwo []         = dayThreeTaskTwo dayThreeDefaultArgs
dayThreeTaskTwo [fileName] = (dayThreeParse fileName) >>= (putStrLn . show . (uncurry dayThreeTaskTwo'))
dayThreeTaskTwo _          = putStrLn "Usage: 3 2 [filename]"

--
-- Day 4
--

dayFourDefaultArgs = ["278384","842795"]

dayFourParse :: [String] -> (Int,Int)
dayFourParse [start,end] = (read start, read end)

dayFourTaskOne' :: Int -> Int -> Int
dayFourTaskOne' start end =
  length $ filter FuelDepot.passwordCriteria [start..end]

dayFourTaskOne [] = dayFourTaskOne dayFourDefaultArgs
dayFourTaskOne [startS,endS] =
  let (start, end) = dayFourParse [startS,endS] in
    putStrLn $ show $ dayFourTaskOne' start end
dayFourTaskOne _ = putStrLn "Usage: 4 1 [start end]"

dayFourTaskTwo' :: Int -> Int -> Int
dayFourTaskTwo' start end =
  length $ filter FuelDepot.enhancedPasswordCriteria [start..end]

dayFourTaskTwo [] = dayFourTaskTwo dayFourDefaultArgs
dayFourTaskTwo [startS,endS] =
  let (start, end) = dayFourParse [startS,endS] in
    putStrLn $ show $ dayFourTaskTwo' start end
dayFourTaskTwo _ = putStrLn "Usage: 4 2 [start end]"
