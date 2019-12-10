module Asteroid where

import Data.List (maximumBy, sortBy)
import Data.Maybe (catMaybes)
import Data.Ord (comparing)

import qualified Data.Set as Set

type Coord = (Int,Int)
type S = Set.Set Coord


parseMap :: String -> Set.Set Coord
parseMap str = Set.fromList $ catMaybes $ indexMap $ lines str
  where indexMap map = concatMap indexRow $ zip map [0..]
        indexRow (row,y) = map (indexSpace y) $ zip row [0..]
        indexSpace y ('#',x) = Just (x,y)
        indexSpace y ('.',x) = Nothing

maxVisibility m = maximumBy (comparing snd) (visibilities m)

visibilities :: Set.Set Coord -> [(Coord,Int)]
visibilities m = map (\c -> (c,countVisible c m)) $ Set.toList m

countVisible :: Coord -> Set.Set Coord -> Int
countVisible from m = Set.size $ visibleFrom from m

visibleFrom :: Coord -> Set.Set Coord -> Set.Set Coord
visibleFrom from m = Set.delete from $ Set.filter (\to -> isVisible from to m) m

isVisible :: Coord -> Coord -> Set.Set Coord -> Bool
isVisible (x1,y1) (x2,y2) m =
  case divisor of
    1 -> True
    d -> not $ any (\n -> Set.member (x1 + (sx*n), y1 + (sy*n)) m) [1..(d-1)]
  where (dx,dy) = (x2-x1, y2-y1)
        divisor = gcd dx dy
        (sx,sy) = (dx `div` divisor, dy `div` divisor)

vaporise from m = if Set.null m then [] else sweep from (Set.toList visible) ++ vaporise from (Set.difference m visible)
  where visible = visibleFrom from m

sweep :: Coord -> [Coord] -> [Coord]
sweep from coords = sortBy (comparing (angleBetween from)) coords

angleBetween :: RealFloat a => Coord -> Coord -> a
angleBetween (x1,y1) (x2,y2) = if angle < 0 then (2*pi) + angle else angle
  where (dx,dy) = (x2-x1, y1-y2) -- note that y is inverted (i.e. going *down*)
        angle = atan2 (fromIntegral dx) (fromIntegral dy)
