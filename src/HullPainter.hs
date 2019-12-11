module HullPainter where

import Data.List (minimumBy, maximumBy)
import qualified Data.Map.Strict as Map
import Data.Ord (comparing)

data Turn = TurnLeft | TurnRight deriving (Eq, Show)
data Direction = North | South | East | West deriving (Eq, Show)

type Coord = (Int, Int)

type Robot = (Coord, Direction, Hull)

type Hull = Map.Map Coord Int

move :: Direction -> Coord -> Coord
move North (x,y) = (x,y+1)
move South (x,y) = (x,y-1)
move East  (x,y) = (x-1,y)
move West  (x,y) = (x+1,y)

turn :: Direction -> Int -> Direction
turn North 0 = West
turn North 1 = East
turn East  0 = North
turn East  1 = South
turn South 0 = East
turn South 1 = West
turn West  0 = South
turn West  1 = North

checkColour :: Coord -> Hull -> Int
checkColour coord hull = Map.findWithDefault 0 coord hull

paint :: Coord -> Int -> Hull -> Hull
paint coord colour hull = Map.insert coord colour hull


run :: [Int] -> [(Int,Robot)]
run inputs = run' inputs ((0,0),North,Map.empty)
  where run' [] (coord,dir,hull) = []
        run' (colourS:turnS:inputs) (coord,dir,hull) = (newColour,st') : run' inputs (coord',dir',hull')
          where hull' = paint coord colourS hull
                dir' = turn dir turnS
                coord' = move dir' coord
                newColour = checkColour coord' hull'
                st' = (coord',dir',hull')

draw (_,_,hull) = draw' (xMin,xMax) (yMin,yMax)
  where coords = Map.keys hull
        xMin = fst $ minimumBy (comparing fst) coords
        xMax = fst $ maximumBy (comparing fst) coords
        yMin = snd $ minimumBy (comparing snd) coords
        yMax = snd $ maximumBy (comparing snd) coords
        showColour c = if c == 0 then ' ' else 'X'
        draw' (xMin,xMax) (yMin,yMax) = unlines $ reverse $ map (\y -> reverse $ map (\x -> showColour (checkColour (x,y) hull)) [xMin..xMax]) [yMin..yMax]
