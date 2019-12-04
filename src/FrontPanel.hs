module FrontPanel where

import Prelude hiding (Left, Right)
import Control.Monad.State
import qualified Data.HashMap.Strict as Map

data Wire = Up Int | Down Int | Left Int | Right Int deriving (Show)

parseWire :: String -> Wire
parseWire ('U':n) = Up    $ read n
parseWire ('D':n) = Down  $ read n
parseWire ('L':n) = Left  $ read n
parseWire ('R':n) = Right $ read n

type Point = ((Int,Int),Int)
type PointMap = Map.HashMap (Int, Int) Int

points :: (Int -> Point) -> Int -> [Point]
points f n = points' f n []
  where points' f 0 acc = acc
        points' f n acc = points' f (n - 1) (f n : acc)

-- For a given bit of wire, return a list of points along its path and update
-- the current position and distance
plot :: (Int,Int,Int) -> Wire -> ([Point],(Int,Int,Int))
plot (x,y,d) (Up n)    =  (points (\i -> ((x, y + i), d + i)) n, (x, y + n, d + n))
plot (x,y,d) (Down n)  =  (points (\i -> ((x, y - i), d + i)) n, (x, y - n, d + n))
plot (x,y,d) (Left n)  =  (points (\i -> ((x - i, y), d + i)) n, (x - n, y, d + n))
plot (x,y,d) (Right n) =  (points (\i -> ((x + i, y), d + i)) n, (x + n, y, d + n))

plotSt :: Wire -> State (Int,Int,Int) [Point]
plotSt wire = state $ \pos -> plot pos wire

plotAll :: [Wire] -> PointMap
plotAll wires = Map.unions $ map Map.fromList $ evalState (forM wires plotSt) (0,0,0)


manhattanDistance :: (Int,Int) -> Int
manhattanDistance (x,y) = (abs x) + (abs y)
