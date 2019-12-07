module Orbit where

import Data.List (find)

type Body = String

data OrbitMap = Node Body [OrbitMap] deriving Show

parseOrbit :: String -> (Body,Body)
parseOrbit orbitS = (com,body) where
  (com,')':body) = break (== ')')  orbitS

totalOrbits :: Body -> [(Body,Body)] -> Int
totalOrbits root orbits = totalOrbits' 0 root
  where totalOrbits' n root = n + sum (map (totalOrbits' (n+1)) (bodies root))
        bodies root = map snd $ filter ((== root) . fst) orbits

pathToRoot :: Body -> [(Body,Body)] -> [Body]
pathToRoot body bodies = case find ((== body) . snd) bodies of
                           Just (parent,_) -> parent : pathToRoot parent bodies
                           Nothing -> []

pathFromRoot body bodies = reverse $ pathToRoot body bodies

pathBetween :: Body -> Body -> [(Body,Body)] -> [Body]
pathBetween bodyOne bodyTwo orbits = (reverse $ drop commonDepth pathToOne) ++ [ancestor] ++ (reverse $ drop commonDepth pathToTwo)
  where pathToOne = pathFromRoot bodyOne orbits
        pathToTwo = pathFromRoot bodyTwo orbits
        commonDepth = length $ takeWhile (uncurry (==)) $ zip pathToOne pathToTwo
        ancestor = pathToOne !! (commonDepth - 1)
