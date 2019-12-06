module Orbit where

import Data.List (find)
import Data.Maybe (fromJust)

type Body = String

data OrbitMap = Node Body [OrbitMap] deriving Show

parseOrbit :: String -> (String,String)
parseOrbit orbitS = (com,body) where
  (com,')':body) = break (== ')')  orbitS

orbits :: Body -> [(Body,Body)] -> Body
orbits body orbits = fst $ fromJust $ find ((== body) . snd) orbits

pathBetweenDown :: Body -> Body -> [(Body,Body)] -> [Body]
pathBetweenDown src dst bodies = reverse $ pathBetween' src dst bodies
  where pathBetween' src dst bodies | src == dst = []
                                    | otherwise  = dst' : pathBetween' src dst' bodies where
                                        dst' = orbits dst bodies

fromList :: Body -> [(Body,Body)] -> OrbitMap
fromList body orbits = Node body (map ((flip fromList) orbits) (directlyOrbiting body orbits))
  where directlyOrbiting body orbits = map snd $ filter ((== body) . fst) orbits

toList :: OrbitMap -> [(Body,Body)]
toList node@(Node src nodes) = map (\dst -> (src,dst)) (directOrbits node) ++ concatMap toList nodes

directOrbits :: OrbitMap -> [Body]
directOrbits (Node _ nodes) = map (\(Node body _) -> body) nodes

allCentreOrbits :: OrbitMap -> [Body]
allCentreOrbits node@(Node _ nodes) = directOrbits node ++ concatMap allCentreOrbits nodes

allOrbits :: OrbitMap -> [Body]
allOrbits node@(Node _ nodes) = allCentreOrbits node ++ concatMap allOrbits nodes

stripGreatestCommonPrefix :: Eq a => [a] -> [a] -> ([a],[a])
stripGreatestCommonPrefix [] ys = ([],ys)
stripGreatestCommonPrefix xs [] = (xs,[])
stripGreatestCommonPrefix xs@(x:xs') ys@(y:ys')  = if x == y then stripGreatestCommonPrefix xs' ys' else (xs, ys)
