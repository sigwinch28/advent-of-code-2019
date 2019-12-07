module Orbit where

import Data.List (find)

type Body = String

data OrbitMap = Node Body [OrbitMap] deriving Show

parseOrbit :: String -> (String,String)
parseOrbit orbitS = (com,body) where
  (com,')':body) = break (== ')')  orbitS

fromList :: Body -> [(Body,Body)] -> OrbitMap
fromList body orbits = Node body (map ((flip fromList) orbits) (directlyOrbiting body orbits))
  where directlyOrbiting body orbits = map snd $ filter ((== body) . fst) orbits

totalDepth :: OrbitMap -> Int
totalDepth node = totalDepth' 0 node
  where totalDepth' n (Node _ nodes) = n + sum (map (totalDepth' (n+1)) nodes)

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
