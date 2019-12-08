module Main where

import Parse

import qualified Fuel
import qualified Intcode
import qualified FrontPanel
import qualified FuelDepot
import qualified Orbit

import Control.Monad.State as State
import Data.Functor ((<&>))
import qualified Data.HashSet as Set
import qualified Data.HashMap.Strict as Map
import Data.List as List
import Data.Ord (comparing)

import qualified System.Environment as Env
import System.IO

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
dispatch 5 1 = dayFiveTaskOne
dispatch 5 2 = dayFiveTaskTwo
dispatch 6 1 = daySixTaskOne
dispatch 6 2 = daySixTaskTwo
dispatch 7 1 = daySevenTaskOne
dispatch 7 2 = daySevenTaskTwo
dispatch d t = \_ -> putStrLn $ "Unknown task: day " ++ (show d) ++ " task " ++ (show t)

--
-- Day One
--

dayOneDefaultArgs = ["data/fuel-calculations/module-masses.txt"]

dayOneParse fileName = (openFileLazy fileName) <&> ( (map read) . lines )

dayOneTaskOne' :: [Int] -> Int
dayOneTaskOne' masses = sum $ map Fuel.fuelForMass masses

dayOneTaskOne []         = dayOneTaskOne dayOneDefaultArgs
dayOneTaskOne [fileName] = (dayOneParse fileName) >>= (print . dayOneTaskOne')
dayOneTaskOne _          = putStrLn "Usage: 1 1 [filename]"

dayOneTaskTwo' :: [Int] -> Int
dayOneTaskTwo' masses = sum $ map Fuel.totalFuel masses

dayOneTaskTwo []         = dayOneTaskTwo dayOneDefaultArgs
dayOneTaskTwo [fileName] = (dayOneParse fileName) >>= (print . dayOneTaskTwo')
dayOneTaskTwo _          = putStrLn "Usage: 1 2 [filename]"

--
-- Day Two
--

dayTwoDefaultArgs = ["data/intcode/gravity-assist.txt"]

dayTwoParse fileName = (openFileLazy fileName) <&> ((map read) . commaDelimited)

dayTwoTaskOne' :: [Int] -> Int
dayTwoTaskOne' prog = head $ fst $ State.execState Intcode.stepUntilInput (prog,0)

dayTwoTaskOne []         = dayTwoTaskOne dayTwoDefaultArgs
dayTwoTaskOne [fileName] = (dayTwoParse fileName) >>= (print . dayTwoTaskOne')
dayTwoTaskOne _          = putStrLn "Usage: 2 1 [filename]"

dayTwoTaskTwo' :: [Int] -> Maybe Int
dayTwoTaskTwo' prog = fmap (\(noun,verb,_) -> ((100 * noun) + verb)) correctProg
  where subst noun verb prog = head prog : ( [noun,verb] ++ (drop 3 prog))
        permute noun verb prog = (noun, verb, subst noun verb prog)
        permutations = concatMap (\noun -> map (\verb -> permute noun verb prog) [1..99]) [1..99]
        results = map (\(n,v,prog) -> (n,v, head $ fst $ State.execState Intcode.stepUntilInput (prog,0))) permutations
        correctProg = List.find (\(_,_,res) -> res == 19690720) results

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
dayThreeTaskOne [fileName] = (dayThreeParse fileName) >>= (print . (uncurry dayThreeTaskOne'))
dayThreeTaskOne _          = putStrLn "Usage: 3 1 [filename]"

dayThreeTaskTwo' :: [FrontPanel.Wire] -> [FrontPanel.Wire] -> Int
dayThreeTaskTwo' a b =
  let aPlot = FrontPanel.plotAll a
      bPlot = FrontPanel.plotAll b
      intersections = Map.intersectionWith (+) aPlot bPlot in
    minimum $ Map.elems intersections

dayThreeTaskTwo :: [String] -> IO ()
dayThreeTaskTwo []         = dayThreeTaskTwo dayThreeDefaultArgs
dayThreeTaskTwo [fileName] = (dayThreeParse fileName) >>= (print . (uncurry dayThreeTaskTwo'))
dayThreeTaskTwo _          = putStrLn "Usage: 3 2 [filename]"

--
-- Day 4
--

dayFourDefaultArgs = ["278384","842795"]

dayFourParse :: [String] -> (Int,Int)
dayFourParse [start,end] = (read start, read end)

dayFourTaskOne' :: Int -> Int -> Int
dayFourTaskOne' start end =
  length $ filter FuelDepot.groupCriteria (FuelDepot.passwordsInRange start end)

dayFourTaskOne [] = dayFourTaskOne dayFourDefaultArgs
dayFourTaskOne [startS,endS] =
  let (start, end) = dayFourParse [startS,endS] in
    print $ dayFourTaskOne' start end
dayFourTaskOne _ = putStrLn "Usage: 4 1 [start end]"

dayFourTaskTwo' :: Int -> Int -> Int
dayFourTaskTwo' start end =
  length $ filter FuelDepot.enhancedGroupCriteria (FuelDepot.passwordsInRange start end)

dayFourTaskTwo [] = dayFourTaskTwo dayFourDefaultArgs
dayFourTaskTwo [startS,endS] =
  let (start, end) = dayFourParse [startS,endS] in
    print $ dayFourTaskTwo' start end
dayFourTaskTwo _ = putStrLn "Usage: 4 2 [start end]"

--
-- Day 5
--

dayFiveParse fileName = (openFileLazy fileName) <&> ((map read) . commaDelimited)

dayFiveTaskOne' :: [Int] -> Int
dayFiveTaskOne' prog = last $ snd $ State.evalState run (prog, 0)
  where run = Intcode.stepUntilInput >>= Intcode.putInput 1 >> Intcode.stepUntilInput

dayFiveTaskOne []         = dayFiveTaskOne ["data/intcode/test.txt"]
dayFiveTaskOne [fileName] = (dayFiveParse fileName) >>= (print . dayFiveTaskOne')
dayFiveTaskOne _          = putStrLn "Usage: 5 1 [filename]"

dayFiveTaskTwo' :: [Int] -> Int
dayFiveTaskTwo' prog = last $ snd $ State.evalState run (prog, 0)
  where run = Intcode.stepUntilInput >>= Intcode.putInput 5 >> Intcode.stepUntilInput

dayFiveTaskTwo []         = dayFiveTaskTwo ["data/intcode/test.txt"]
dayFiveTaskTwo [fileName] = (dayFiveParse fileName) >>= (print . dayFiveTaskTwo')
dayFiveTaskTwo _          = putStrLn "Usage: 5 2 [filename]"

--
-- Day 6
--
daySixParse fileName = (openFileLazy fileName) <&> ((map Orbit.parseOrbit) . lines)

daySixTaskOne' orbits = Orbit.totalOrbits "COM" orbits

daySixTaskOne []         = daySixTaskOne ["data/orbits/mercury.txt"]
daySixTaskOne [fileName] = (daySixParse fileName) >>= (print . daySixTaskOne')
daySixTaskOne _          = putStrLn "Usage: 6 1 [filename]"

daySixTaskTwo' orbits = (\n -> n - 1) $ length $ Orbit.pathBetween "YOU" "SAN" orbits

daySixTaskTwo []         = daySixTaskTwo ["data/orbits/mercury.txt"]
daySixTaskTwo [fileName] = (daySixParse fileName) >>= (print . daySixTaskTwo')
daySixTaskTwo _          = putStrLn "Usage: 6 2 [filename]"

--
-- Day 7
--
daySevenDefaultArgs = ["data/intcode/gravity-assist.txt"]

daySevenParse fileName = (openFileLazy fileName) <&> ((map read) . commaDelimited)

daySevenTaskOne' :: [Int] -> Int
daySevenTaskOne' prog = maximum $ map runAmps perms
  where runAmps phases = head $ foldl (\i n -> run (n:i)) [0] phases
        run inputs = snd $ State.evalState (Intcode.stepUntilInput >>= Intcode.stepWithInputs inputs) (prog,0)
        perms = permutations [0..4]

daySevenTaskOne [] = daySevenTaskOne ["data/intcode/thrusters.txt"]
daySevenTaskOne [fileName] = (daySevenParse fileName) >>= (print . daySevenTaskOne')
daySevenTaskOne _ = putStrLn "Usage: 7 1 [filename]"

--daySevenTaskTwo' :: [Int] -> Int
daySevenTaskTwo' prog = maximum $ map (head . (runUntilHalt [0]) . initAmps) perms
  where perms = permutations [5..9]
        initAmps perms = map initAmp perms
        initAmp perm = (\((sig,_),st) -> (sig,st)) $ State.runState (Intcode.stepUntilInput >>= Intcode.stepWithInputs [perm]) (prog,0)
        runUntilHalt inputs ((Intcode.Halted,_):_) = inputs
        runUntilHalt inputs amps = let (amps',outputs) = runAmps inputs amps in runUntilHalt outputs amps'
        runAmps inputs amps = runAmps' inputs amps []
        runAmps' inputs [] acc = (reverse acc, inputs)
        runAmps' inputs ((sig,st):amps) acc = runAmps' outputs amps ((sig',amp):acc)
          where ((sig',outputs),amp) = State.runState (Intcode.stepWithInputs inputs (sig,[])) st

daySevenTaskTwo [] = daySevenTaskTwo ["data/intcode/thrusters.txt"]
daySevenTaskTwo [fileName] = (daySevenParse fileName) >>= (print . daySevenTaskTwo')
daySevenTaskTwo _ = putStrLn "Usage: 7 1 [filename]"
