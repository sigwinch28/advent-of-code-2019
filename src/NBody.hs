module NBody where

import Data.Maybe (fromJust)
import Data.List (elemIndex, mapAccumL)
import qualified Data.Set as Set

type Component = (Int,Int)
type Body = (Component,Component,Component)

newBody :: Int -> Int -> Int -> Body
newBody px py pz = ((px,0),(py,0),(pz,0))

applyGravity :: Component -> Component -> (Component,Component)
applyGravity (p1,v1) (p2,v2) = ( (p1, v1+d1), (p2, v2+d2) )
  where (d1,  d2 ) = velocityDelta
        velocityDelta | p1 < p2  = (1 , -1)
                      | p1 == p2 = (0 , 0 )
                      | p1 > p2  = (-1, 1 )

applyVelocity :: Component -> Component
applyVelocity (p,v) = (p+v, v)

step :: [Component] -> [Component]
step [] = []
step (comp:comps) = (applyVelocity comp') : step comps'
  where (comp',comps') = mapAccumL applyGravity comp comps

steps :: [Component] -> [[Component]]
steps comps = iterate step comps

bodySteps :: [Body] -> [[Body]]
bodySteps bodies = zipWith3 zip3 (steps xs) (steps ys) (steps zs)
  where (xs,ys,zs) = unzip3 bodies

period :: Ord a => [a] -> Int
period (state:states) = (+1) $ fromJust $ elemIndex state states

bodiesPeriod :: [Body] -> Int
bodiesPeriod bodies = lcm (lcm x y) z
  where x = period $ steps xs
        y = period $ steps ys
        z = period $ steps zs
        (xs, ys, zs) = unzip3 bodies

energy :: Body -> Int
energy ((px,vx),(py,vy),(pz,vz)) = potentialEnergy * kineticEnergy
  where potentialEnergy = (abs px) + (abs py) + (abs pz)
        kineticEnergy   = (abs vx) + (abs vy) + (abs vz)
