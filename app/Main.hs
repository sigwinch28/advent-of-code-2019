module Main where

import Parse

import qualified Fuel
import qualified Intcode
import qualified FrontPanel
import qualified FuelDepot
import qualified Orbit
import qualified SpaceImage
import qualified Asteroid

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
dispatch 8 1 = dayEightTaskOne
dispatch 8 2 = dayEightTaskTwo
dispatch 9 1 = dayNineTaskOne
dispatch 9 2 = dayNineTaskTwo
dispatch 10 1 = dayTenTaskOne
dispatch 10 2 = dayTenTaskTwo
dispatch d t = \_ -> putStrLn $ "Unknown task: day " ++ (show d) ++ " task " ++ (show t)

--
-- Day One
--

dayOneDefaultArgs = ["data/fuel-calculations/module-masses.txt"]

dayOneParse fileName = (openFileLazy fileName) <&> ( (map read) . lines )

dayOneTaskOne' masses = sum $ map Fuel.fuelForMass masses

dayOneTaskOne []         = dayOneTaskOne dayOneDefaultArgs
dayOneTaskOne [fileName] = (dayOneParse fileName) >>= (print . dayOneTaskOne')
dayOneTaskOne _          = putStrLn "Usage: 1 1 [filename]"

dayOneTaskTwo' masses = sum $ map Fuel.totalFuel masses

dayOneTaskTwo []         = dayOneTaskTwo dayOneDefaultArgs
dayOneTaskTwo [fileName] = (dayOneParse fileName) >>= (print . dayOneTaskTwo')
dayOneTaskTwo _          = putStrLn "Usage: 1 2 [filename]"

--
-- Day Two
--

dayTwoDefaultArgs = ["data/intcode/gravity-assist.txt"]

dayTwoParse fileName = (openFileLazy fileName) <&> ((map read) . commaDelimited)

dayTwoTaskOne' prog = head $ fst $ Intcode.runMem (prog,0)

dayTwoTaskOne []         = dayTwoTaskOne dayTwoDefaultArgs
dayTwoTaskOne [fileName] = (dayTwoParse fileName) >>= (print . dayTwoTaskOne')
dayTwoTaskOne _          = putStrLn "Usage: 2 1 [filename]"

dayTwoTaskTwo' prog = fmap (\(noun,verb,_) -> ((100 * noun) + verb)) correctProg
  where subst noun verb prog = head prog : ( [noun,verb] ++ (drop 3 prog))
        permute noun verb prog = (noun, verb, subst noun verb prog)
        permutations = concatMap (\noun -> map (\verb -> permute noun verb prog) [1..99]) [1..99]
        results = map (\(n,v,prog) -> (n,v, head $ fst $ Intcode.runMem (prog,0))) permutations
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

dayThreeTaskOne' a b =
  let aCoords = Map.keysSet $ FrontPanel.plotAll a
      bCoords = Map.keysSet $ FrontPanel.plotAll b
      intersections = Set.toList $ Set.intersection aCoords bCoords
      distances = map FrontPanel.manhattanDistance intersections in
    minimum distances

dayThreeTaskOne []         = dayThreeTaskOne dayThreeDefaultArgs
dayThreeTaskOne [fileName] = (dayThreeParse fileName) >>= (print . (uncurry dayThreeTaskOne'))
dayThreeTaskOne _          = putStrLn "Usage: 3 1 [filename]"

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

dayFourParse [start,end] = (read start, read end)

dayFourTaskOne' start end =  length $ filter FuelDepot.groupCriteria (FuelDepot.passwordsInRange start end)

dayFourTaskOne []            = dayFourTaskOne dayFourDefaultArgs
dayFourTaskOne [startS,endS] = print $ (uncurry dayFourTaskOne') $ dayFourParse [startS,endS]
dayFourTaskOne _             = putStrLn "Usage: 4 1 [start end]"

dayFourTaskTwo' start end =
  length $ filter FuelDepot.enhancedGroupCriteria (FuelDepot.passwordsInRange start end)

dayFourTaskTwo []            = dayFourTaskTwo dayFourDefaultArgs
dayFourTaskTwo [startS,endS] = print $ (uncurry dayFourTaskTwo') $ dayFourParse [startS,endS]
dayFourTaskTwo _             = putStrLn "Usage: 4 2 [start end]"

--
-- Day 5
--

dayFiveDefaultArgs = ["data/intcode/test.txt"]

dayFiveParse fileName = (openFileLazy fileName) <&> ((map read) . commaDelimited)

dayFiveTaskOne' prog = last $ Intcode.runProg [1] prog

dayFiveTaskOne []         = dayFiveTaskOne dayFiveDefaultArgs
dayFiveTaskOne [fileName] = (dayFiveParse fileName) >>= (print . dayFiveTaskOne')
dayFiveTaskOne _          = putStrLn "Usage: 5 1 [filename]"

dayFiveTaskTwo' prog = last $ Intcode.runProg [5] prog

dayFiveTaskTwo []         = dayFiveTaskTwo dayFiveDefaultArgs
dayFiveTaskTwo [fileName] = (dayFiveParse fileName) >>= (print . dayFiveTaskTwo')
dayFiveTaskTwo _          = putStrLn "Usage: 5 2 [filename]"

--
-- Day 6
--

daySixDefaultArgs = ["data/orbits/mercury.txt"]

daySixParse fileName = (openFileLazy fileName) <&> ((map Orbit.parseOrbit) . lines)

daySixTaskOne' orbits = Orbit.totalOrbits "COM" orbits

daySixTaskOne []         = daySixTaskOne daySixDefaultArgs
daySixTaskOne [fileName] = (daySixParse fileName) >>= (print . daySixTaskOne')
daySixTaskOne _          = putStrLn "Usage: 6 1 [filename]"

daySixTaskTwo' orbits = (\n -> n - 1) $ length $ Orbit.pathBetween "YOU" "SAN" orbits

daySixTaskTwo []         = daySixTaskTwo daySixDefaultArgs
daySixTaskTwo [fileName] = (daySixParse fileName) >>= (print . daySixTaskTwo')
daySixTaskTwo _          = putStrLn "Usage: 6 2 [filename]"

--
-- Day 7
--
daySevenDefaultArgs = ["data/intcode/thrusters.txt"]

daySevenParse fileName = (openFileLazy fileName) <&> ((map read) . commaDelimited)

daySevenTaskOne' prog = maximum $ map runAmps perms
  where runAmps phases = head $ foldl (\i n -> run (n:i)) [0] phases
        run inputs = Intcode.runProg inputs prog
        perms = permutations [0..4]

daySevenTaskOne []         = daySevenTaskOne daySevenDefaultArgs
daySevenTaskOne [fileName] = (daySevenParse fileName) >>= (print . daySevenTaskOne')
daySevenTaskOne _          = putStrLn "Usage: 7 1 [filename]"

daySevenTaskTwo' prog = maximum $ map runAmps perms
  where perms = permutations [5..9]
        runAmps [p0,p1,p2,p3,p4] = last outE
          where outA = Intcode.runProg (p0:0:outE) prog
                outB = Intcode.runProg (p1:outA)   prog
                outC = Intcode.runProg (p2:outB)   prog
                outD = Intcode.runProg (p3:outC)   prog
                outE = Intcode.runProg (p4:outD)   prog

daySevenTaskTwo []         = daySevenTaskTwo daySevenDefaultArgs
daySevenTaskTwo [fileName] = (daySevenParse fileName) >>= (print . daySevenTaskTwo')
daySevenTaskTwo _          = putStrLn "Usage: 7 1 [filename]"

--
-- Day 8
--

dayEightDefaultArgs = ["data/images/bios-password.txt","25","6"]

dayEightParse fileName = (openFileLazy fileName) <&> (head . lines)

dayEightTaskOne' width height image = calcResult $ snd $ minimumBy (comparing fst) $ zip (map (countInstances SpaceImage.Black) layers) layers
  where layers = SpaceImage.parseImage width height image
        calcResult layer = (countInstances SpaceImage.White layer) * (countInstances SpaceImage.Transparent layer)
        countInstances pix layer = length $ concatMap (filter (== pix)) layer

dayEightTaskOne []                      = dayEightTaskOne dayEightDefaultArgs
dayEightTaskOne [fileName,width,height] = (dayEightParse fileName) >>= (print . dayEightTaskOne' (read width) (read height))
dayEightTaskOne _                       = putStrLn "Usage: 8 1 [filename width height]"

dayEightTaskTwo' width height image = SpaceImage.showLayer $ SpaceImage.flattenImage $ SpaceImage.parseImage width height image

dayEightTaskTwo []                      = dayEightTaskTwo dayEightDefaultArgs
dayEightTaskTwo [fileName,width,height] = (dayEightParse fileName) >>= (putStr . dayEightTaskTwo' (read width) (read height))
dayEightTaskTwo _                       = putStrLn "Usage: 8 2 [filename width height]"

--
-- Day 9
--

dayNineDefaultArgs = ["data/intcode/boost.txt"]

dayNineParse fileName = (openFileLazy fileName) <&> ((map read) . commaDelimited)

dayNineTaskOne' prog = last $ Intcode.runProg [1] prog

dayNineTaskOne []         = dayNineTaskOne dayNineDefaultArgs
dayNineTaskOne [fileName] = (dayNineParse fileName) >>= (print . dayNineTaskOne')

dayNineTaskTwo' prog = last $ Intcode.runProg [2] prog

dayNineTaskTwo []         = dayNineTaskTwo dayNineDefaultArgs
dayNineTaskTwo [fileName] = (dayNineParse fileName) >>= (print . dayNineTaskTwo')

--
-- Day 10
--

dayTenDefaultArgs = ["data/asteroids/station.txt"]

dayTenParse fileName = (openFileLazy fileName) <&> (Asteroid.parseMap)

dayTenTaskOne' m = snd $ Asteroid.maxVisibility m

dayTenTaskOne []         = dayTenTaskOne dayTenDefaultArgs
dayTenTaskOne [fileName] = (dayTenParse fileName) >>= (print . dayTenTaskOne')

dayTenTaskTwo' m = (100*x) + y
  where station = fst $ Asteroid.maxVisibility m
        (x,y) = (Asteroid.vaporise station m) !! (200-1)

dayTenTaskTwo []         = dayTenTaskTwo dayTenDefaultArgs
dayTenTaskTwo [fileName] = (dayTenParse fileName) >>= (print . dayTenTaskTwo')
